defmodule XprofGuiLiveviewWeb.MonitoringLive do
  use XprofGuiLiveviewWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Start periodic updates when LiveView connects
      :timer.send_interval(1000, self(), :update_status)
      :timer.send_interval(2000, self(), :update_functions)
      :timer.send_interval(1000, self(), :update_captures)
      :timer.send_interval(30000, self(), :update_favourites)
    end

    {:ok,
     socket
     |> assign(:query, "")
     |> assign(:functions, [])
     |> assign(:monitored_functions, [])
     |> assign(:position, -1)
     |> assign(:history_position, -1)
     |> assign(:input_type, :search)
     |> assign(:trace_status, "paused")
     |> assign(:mode, nil)
     |> assign(:grid, 1)
     |> assign(:favourites, [])
     |> assign(:recent_queries, [])
     |> assign(:capture_data, %{})
     |> fetch_initial_data()}
  end

  @impl true
  def handle_event("update_query", %{"query" => query}, socket) do
    # Handle query input changes
    suggestions = case socket.assigns.input_type do
      :search ->
        # Use xprof_core autocomplete for search mode
        if String.length(query) >= 2 do
          fetch_autocomplete_functions(query)
        else
          []
        end

      :favourites ->
        # Filter favourites based on query
        if String.length(query) >= 2 do
          filter_favourites(socket.assigns.favourites, query)
        else
          socket.assigns.favourites
        end
    end

    # Don't show autocomplete if there's only one match that equals the current query
    # (i.e., append value would be empty, so dropdown would be useless)
    filtered_suggestions = case suggestions do
      [single_match] ->
        match_value = case single_match do
          %{value: val} -> val
          val when is_binary(val) -> val
          _ -> to_string(single_match)
        end
        # If the only match equals current query, hide dropdown
        if match_value == query, do: [], else: suggestions

      _ ->
        suggestions
    end

    # Reset history position when user types (they're editing, not navigating history)
    {:noreply, assign(socket, query: query, functions: filtered_suggestions, position: -1, history_position: -1)}
  end

  @impl true
  def handle_event("submit_query", %{"query" => query}, socket) do
    # Validate query before processing
    case validate_query(query) do
      {:ok, validated_query} ->
        # Add to recent queries history (only on submit, not on every keystroke)
        recent_queries = socket.assigns.recent_queries || []
        last_query = if recent_queries == [], do: nil, else: hd(recent_queries)

        updated_recent =
          if String.length(validated_query) > 0 and validated_query != last_query do
            [validated_query | Enum.take(recent_queries, 19)]  # Keep last 20
          else
            recent_queries
          end

        # Start monitoring the function
        case monitor_function(validated_query) do
          :ok ->
            monitored = fetch_monitored_functions()

            {:noreply,
             socket
             |> assign(
               query: "",
               functions: [],
               monitored_functions: monitored,
               recent_queries: updated_recent,
               history_position: -1
             )
             |> put_flash(:info, "Started monitoring: #{validated_query}")}

          {:error, :already_traced} ->
            {:noreply,
             socket
             |> assign(recent_queries: updated_recent)
             |> put_flash(:info, "Function is already being monitored")}

          {:error, reason} ->
            {:noreply,
             socket
             |> assign(recent_queries: updated_recent)
             |> put_flash(:error, "Failed to monitor: #{inspect(reason)}")}
        end

      {:error, message} ->
        {:noreply, put_flash(socket, :error, message)}
    end
  end

  @impl true
  def handle_event("demonitor", %{"mfa" => mfa_str}, socket) do
    # Stop monitoring the function
    mfa = parse_mfa(mfa_str)

    case demonitor_function(mfa) do
      :ok ->
        monitored = fetch_monitored_functions()
        {:noreply,
         socket
         |> assign(monitored_functions: monitored)
         |> put_flash(:info, "Stopped monitoring: #{format_mfa(mfa)}")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to stop monitoring: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("add_to_favourites", %{"query" => query}, socket) do
    # Add query to favourites
    case add_favourite(query) do
      :ok ->
        favourites = fetch_favourites()
        {:noreply,
         socket
         |> assign(favourites: favourites)
         |> put_flash(:info, "Added to favourites")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to add favourite: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("remove_from_favourites", %{"query" => query}, socket) do
    # Remove query from favourites
    case remove_favourite(query) do
      :ok ->
        favourites = fetch_favourites()
        {:noreply,
         socket
         |> assign(favourites: favourites)
         |> put_flash(:info, "Removed from favourites")}

      {:error, :not_found} ->
        {:noreply, put_flash(socket, :info, "Favourite not found")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to remove favourite: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("toggle_trace", _params, socket) do
    new_status = case socket.assigns.trace_status do
      "paused" -> toggle_trace_status(:all)
      _ -> toggle_trace_status(:pause)
    end

    {:noreply, assign(socket, trace_status: new_status)}
  end

  @impl true
  def handle_event("toggle_input_type", _params, socket) do
    new_type = case socket.assigns.input_type do
      :search -> :favourites
      :favourites -> :search
    end

    {:noreply, assign(socket, input_type: new_type, query: "", functions: [])}
  end

  @impl true
  def handle_event("switch_grid", %{"grid" => grid_str}, socket) do
    grid = String.to_integer(grid_str)
    {:noreply, assign(socket, grid: grid)}
  end

  @impl true
  def handle_event("start_capture", %{"mfa" => mfa_str}, socket) do
    mfa = parse_mfa(mfa_str)

    # Start capture with default threshold (0ms) and limit (100 calls)
    case start_capture(mfa, 0, 100) do
      {:ok, capture_id} ->
        {:noreply,
         socket
         |> put_flash(:info, "Started capturing calls for #{format_mfa(mfa)} (ID: #{capture_id})")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to start capture: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("stop_capture", %{"mfa" => mfa_str}, socket) do
    mfa = parse_mfa(mfa_str)

    case stop_capture(mfa) do
      :ok ->
        {:noreply,
         socket
         |> put_flash(:info, "Stopped capturing calls for #{format_mfa(mfa)}")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to stop capture: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("view_capture", %{"mfa" => mfa_str}, socket) do
    mfa = parse_mfa(mfa_str)

    # Fetch captured data and store in socket state
    case fetch_captured_data(mfa, 0) do
      {:ok, spec, items} ->
        capture_data = Map.put(socket.assigns.capture_data, mfa_str, %{spec: spec, items: items})
        {:noreply, assign(socket, capture_data: capture_data)}

      {:error, :not_found} ->
        {:noreply, put_flash(socket, :info, "No captured data for #{format_mfa(mfa)}")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to fetch capture data: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("select_function", %{"index" => index_str}, socket) do
    index = String.to_integer(index_str)
    functions = socket.assigns.functions

    if index >= 0 and index < length(functions) do
      function = Enum.at(functions, index)
      # Extract the value from the function map (or use the string directly if it's not a map)
      new_value = case function do
        %{value: val} -> val
        val when is_binary(val) -> val
        _ -> to_string(function)
      end
      # Append the selected value to the existing query
      updated_query = socket.assigns.query <> new_value

      # Regenerate autocomplete suggestions based on the updated query
      new_suggestions = case socket.assigns.input_type do
        :search ->
          if String.length(updated_query) >= 2 do
            fetch_autocomplete_functions(updated_query)
          else
            []
          end
        :favourites ->
          if String.length(updated_query) >= 2 do
            filter_favourites(socket.assigns.favourites, updated_query)
          else
            socket.assigns.favourites
          end
      end

      {:noreply, assign(socket, query: updated_query, functions: new_suggestions, position: -1)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("key_down", %{"key" => key}, socket) do
    handle_key_event(key, socket)
  end

  @impl true
  def handle_info(:update_status, socket) do
    status = fetch_trace_status()
    {:noreply, assign(socket, trace_status: status)}
  end

  @impl true
  def handle_info(:update_functions, socket) do
    monitored = fetch_monitored_functions()

    # Fetch statistics for each monitored function
    monitored_with_stats = Enum.map(monitored, fn mon ->
      stats = fetch_function_stats(mon.mfa)
      Map.put(mon, :stats, stats)
    end)

    {:noreply, assign(socket, monitored_functions: monitored_with_stats)}
  end

  @impl true
  def handle_info(:update_favourites, socket) do
    favourites = fetch_favourites()
    {:noreply, assign(socket, favourites: favourites)}
  end

  @impl true
  def handle_info(:update_captures, socket) do
    # Update all active captures with fresh data
    updated_capture_data =
      socket.assigns.capture_data
      |> Enum.map(fn {mfa_str, _current_data} ->
        mfa = parse_mfa(mfa_str)
        case fetch_captured_data(mfa, 0) do
          {:ok, spec, items} ->
            {mfa_str, %{spec: spec, items: items}}
          {:error, _reason} ->
            # Keep existing data if fetch fails
            {mfa_str, socket.assigns.capture_data[mfa_str]}
        end
      end)
      |> Map.new()

    {:noreply, assign(socket, capture_data: updated_capture_data)}
  end

  # Private functions for API calls to xprof_core

  defp fetch_initial_data(socket) do
    socket
    |> assign(:mode, fetch_mode())
    |> assign(:trace_status, fetch_trace_status())
    |> assign(:monitored_functions, fetch_monitored_functions())
    |> assign(:favourites, fetch_favourites())
  end

  defp fetch_mode do
    # Call xprof_core to get the mode (erlang/elixir)
    try do
      case :xprof_core.get_mode() do
        {:ok, mode} -> Atom.to_string(mode)
        _ -> "unknown"
      end
    rescue
      e ->
        Logger.error("Failed to fetch mode: #{inspect(e)}")
        "unknown"
    end
  end

  defp fetch_trace_status do
    # Call xprof_core to get trace status
    try do
      case :xprof_core.get_trace_status() do
        {:ok, :running} -> "running"
        {:ok, :paused} -> "paused"
        _ -> "paused"
      end
    rescue
      e ->
        Logger.error("Failed to fetch trace status: #{inspect(e)}")
        "paused"
    end
  end

  defp fetch_monitored_functions do
    # Get list of monitored functions from xprof_core
    try do
      monitored = :xprof_core.get_all_monitored()
      Enum.map(monitored, &format_monitored_function/1)
    rescue
      e ->
        Logger.error("Failed to fetch monitored functions: #{inspect(e)}")
        []
    end
  end

  defp fetch_function_stats(mfa) when is_tuple(mfa) do
    # Get statistics for a specific function
    # xprof_core:get_data/2 takes MFA and timestamp
    # Use timestamp 0 to get all available data
    try do
      case :xprof_core.get_data(mfa, 0) do
        {:error, :not_found} ->
          nil

        data when is_list(data) ->
          # Data is a list of snapshots: [[{time, TS}, {min, V}, {max, V}, ...], ...]
          # Get the most recent snapshot (last in list)
          case List.last(data) do
            nil -> nil
            snapshot -> format_stats_snapshot(snapshot)
          end

        _ ->
          nil
      end
    rescue
      e ->
        Logger.error("Failed to fetch stats for #{inspect(mfa)}: #{inspect(e)}")
        nil
    end
  end

  defp fetch_function_stats(_), do: nil

  defp format_stats_snapshot(snapshot) when is_list(snapshot) do
    # Convert snapshot proplist to map for easier access in templates
    # snapshot = [{time, TS}, {count, N}, {min, V}, {mean, V}, {max, V}, {p50, V}, ...]
    Enum.into(snapshot, %{})
  end

  defp format_stats_snapshot(_), do: nil

  defp fetch_favourites do
    # Get favourites from ETS-based store
    try do
      XprofGuiLiveview.FavouritesStore.list()
    rescue
      e ->
        Logger.error("Failed to fetch favourites: #{inspect(e)}")
        []
    end
  end

  defp fetch_autocomplete_functions(query) do
    # Use xprof_core expand_query for autocomplete
    try do
      case :xprof_core.expand_query(query) do
        {_prefix, matches} ->
          Enum.map(matches, fn
            {expand, label} -> %{value: to_string(expand), label: to_string(label)}
            expand when is_binary(expand) -> %{value: to_string(expand), label: to_string(expand)}
          end)

        _ -> []
      end
    rescue
      e ->
        Logger.error("Failed to fetch autocomplete functions for query '#{query}': #{inspect(e)}")
        []
    end
  end

  defp filter_favourites(favourites, query) do
    # Filter favourites list by query string
    query_lower = String.downcase(query)
    favourites
    |> Enum.filter(fn fav ->
      String.contains?(String.downcase(to_string(fav)), query_lower)
    end)
    |> Enum.map(fn fav -> %{value: to_string(fav), label: to_string(fav)} end)
  end

  defp monitor_function(query) do
    # Start monitoring a function using the query string
    try do
      :xprof_core.monitor_pp(String.to_charlist(query))
    rescue
      e ->
        Logger.error("Failed to monitor function '#{query}': #{inspect(e)}")
        {:error, e}
    end
  end

  defp demonitor_function(mfa) when is_tuple(mfa) do
    # Stop monitoring a function
    try do
      :xprof_core.demonitor(mfa)
    rescue
      e ->
        Logger.error("Failed to demonitor function #{inspect(mfa)}: #{inspect(e)}")
        {:error, e}
    end
  end

  defp add_favourite(query) when is_binary(query) do
    # Add to persistent favourites store
    try do
      XprofGuiLiveview.FavouritesStore.add(query)
    rescue
      e ->
        Logger.error("Failed to add favourite '#{query}': #{inspect(e)}")
        {:error, e}
    end
  end

  defp remove_favourite(query) when is_binary(query) do
    # Remove from persistent favourites store
    try do
      XprofGuiLiveview.FavouritesStore.remove(query)
    rescue
      e ->
        Logger.error("Failed to remove favourite '#{query}': #{inspect(e)}")
        {:error, e}
    end
  end

  defp toggle_trace_status(spec) do
    try do
      :xprof_core.trace(spec)
      case spec do
        :all -> "running"
        :pause -> "paused"
      end
    rescue
      e ->
        Logger.error("Failed to toggle trace status to #{inspect(spec)}: #{inspect(e)}")
        "paused"
    end
  end

  defp start_capture(mfa, threshold, limit) when is_tuple(mfa) do
    # Start capturing function calls
    # xprof_core:capture/3 returns {:ok, CaptureId} or {:error, Reason}
    try do
      :xprof_core.capture(mfa, threshold, limit)
    rescue
      e ->
        Logger.error("Failed to start capture for #{inspect(mfa)}: #{inspect(e)}")
        {:error, e}
    end
  end

  defp stop_capture(mfa) when is_tuple(mfa) do
    # Stop capturing function calls
    try do
      :xprof_core.capture_stop(mfa)
    rescue
      e ->
        Logger.error("Failed to stop capture for #{inspect(mfa)}: #{inspect(e)}")
        {:error, e}
    end
  end

  defp fetch_captured_data(mfa, offset) when is_tuple(mfa) do
    # Fetch captured call data using get_captured_data_pp
    # xprof_core handles formatting
    # Returns: {:ok, {CaptureId, Threshold, Limit, HasMore}, Items} or {:error, Reason}
    # Items: [[{id, Index}, {pid, FormattedPid}, {call_time, Ms}, {args, FormattedArgs}, {res, FormattedRes}], ...]
    try do
      case :xprof_core.get_captured_data_pp(mfa, offset) do
        {:ok, {capture_id, threshold, limit, has_more}, items} ->
          spec = %{
            capture_id: capture_id,
            threshold: threshold,
            limit: limit,
            has_more: has_more
          }

          # Convert proplists to maps
          formatted_items =
            Enum.map(items, fn proplist ->
              %{
                id: :proplists.get_value(:id, proplist),
                pid: to_string(:proplists.get_value(:pid, proplist)),
                call_time: :proplists.get_value(:call_time, proplist),
                args: to_string(:proplists.get_value(:args, proplist)),
                res: to_string(:proplists.get_value(:res, proplist))
              }
            end)

          {:ok, spec, formatted_items}

        {:error, reason} ->
          {:error, reason}
      end
    rescue
      e ->
        Logger.error("Failed to fetch captured data for #{inspect(mfa)}: #{inspect(e)}")
        {:error, e}
    end
  end

  defp validate_query(query) when is_binary(query) do
    trimmed = String.trim(query)

    cond do
      String.length(trimmed) == 0 ->
        {:error, "Query cannot be empty"}

      String.length(trimmed) > 500 ->
        {:error, "Query too long (max 500 characters)"}

      # Basic sanity check: query should contain some alphanumeric characters
      not String.match?(trimmed, ~r/[a-zA-Z0-9_]/) ->
        {:error, "Query must contain valid characters (letters, numbers, or underscore)"}

      true ->
        {:ok, trimmed}
    end
  end

  defp validate_query(_), do: {:error, "Invalid query format"}

  defp format_monitored_function({mfa, query}) when is_tuple(mfa) do
    # Format the monitored function data for display
    # xprof_core.get_all_monitored() returns list of {MFA, Query} tuples
    %{
      mfa: mfa,
      query: to_string(query),
      mfa_str: serialize_mfa(mfa),
      stats: nil  # Initialize to nil - will be populated by :update_functions timer
    }
  end

  defp format_monitored_function(_) do
    # Fallback for unexpected format
    %{mfa: nil, query: "", mfa_str: "", stats: nil}
  end

  defp handle_key_event("ArrowUp", socket) do
    # If there are autocomplete suggestions, navigate through them
    if length(socket.assigns.functions) > 0 do
      new_position = max(socket.assigns.position - 1, -1)
      {:noreply, assign(socket, position: new_position)}
    else
      # Navigate through recent queries (like command history)
      recent_queries = socket.assigns.recent_queries

      if length(recent_queries) > 0 do
        current_pos = socket.assigns.history_position
        new_pos = min(current_pos + 1, length(recent_queries) - 1)

        # Update query with the selected history item
        query = if new_pos >= 0 and new_pos < length(recent_queries) do
          Enum.at(recent_queries, new_pos)
        else
          ""
        end

        {:noreply, assign(socket, query: query, history_position: new_pos)}
      else
        {:noreply, socket}
      end
    end
  end

  defp handle_key_event("ArrowDown", socket) do
    # If there are autocomplete suggestions, navigate through them
    if length(socket.assigns.functions) > 0 do
      max_pos = length(socket.assigns.functions) - 1
      new_position = min(socket.assigns.position + 1, max_pos)
      {:noreply, assign(socket, position: new_position)}
    else
      # Navigate through recent queries (like command history)
      recent_queries = socket.assigns.recent_queries
      current_pos = socket.assigns.history_position

      if current_pos > 0 do
        new_pos = current_pos - 1
        query = Enum.at(recent_queries, new_pos)
        {:noreply, assign(socket, query: query, history_position: new_pos)}
      else if current_pos == 0 do
        # Go back to empty query
        {:noreply, assign(socket, query: "", history_position: -1)}
      else
        {:noreply, socket}
      end
      end
    end
  end

  defp handle_key_event("Enter", socket) do
    # Select from autocomplete if something is highlighted
    # The form submit will handle the actual submission
    if socket.assigns.position >= 0 do
      # Select highlighted function and update query
      function = Enum.at(socket.assigns.functions, socket.assigns.position)
      # Extract the value from the function map
      new_value = case function do
        %{value: val} -> val
        val when is_binary(val) -> val
        _ -> to_string(function)
      end
      # Append the selected value to the existing query
      updated_query = socket.assigns.query <> new_value

      # Regenerate autocomplete suggestions based on the updated query
      new_suggestions = case socket.assigns.input_type do
        :search ->
          if String.length(updated_query) >= 2 do
            fetch_autocomplete_functions(updated_query)
          else
            []
          end
        :favourites ->
          if String.length(updated_query) >= 2 do
            filter_favourites(socket.assigns.favourites, updated_query)
          else
            socket.assigns.favourites
          end
      end

      {:noreply, assign(socket, query: updated_query, functions: new_suggestions, position: -1)}
    else
      # Let the form submit handle it
      {:noreply, socket}
    end
  end

  defp handle_key_event(_key, socket) do
    {:noreply, socket}
  end

  # Template helper functions

  def get_placeholder(:search, mode, _favourites) do
    case mode do
      "erlang" -> "e.g., lists:map/2 or Module:function/arity"
      "elixir" -> "e.g., Enum.map/2 or Module.function/arity"
      _ -> "Enter function to monitor..."
    end
  end

  def get_placeholder(:favourites, _mode, favourites) do
    if length(favourites) > 0 do
      "Search favourites..."
    else
      "No favourites yet"
    end
  end

  def format_mfa({mod, fun, arity}) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    "#{mod}:#{fun}/#{arity}"
  end

  def format_mfa(mfa) when is_list(mfa) and length(mfa) == 3 do
    [mod, fun, arity] = mfa
    "#{mod}:#{fun}/#{arity}"
  end

  def format_mfa(_), do: "unknown"

  def format_timestamp(timestamp) when is_integer(timestamp) do
    # Convert Unix timestamp (seconds) to formatted time
    datetime = DateTime.from_unix!(timestamp)
    Calendar.strftime(datetime, "%H:%M:%S")
  end

  def format_timestamp(_), do: ""

  defp parse_mfa(mfa_str) when is_binary(mfa_str) do
    # Parse "module:function/arity" string into {module, function, arity} tuple
    case String.split(mfa_str, [":", "/"]) do
      [mod_str, fun_str, arity_str] ->
        {String.to_atom(mod_str), String.to_atom(fun_str), String.to_integer(arity_str)}

      _ ->
        nil
    end
  end

  defp serialize_mfa({mod, fun, arity}) do
    # Convert MFA tuple to string for HTML attributes
    "#{mod}:#{fun}/#{arity}"
  end
end
