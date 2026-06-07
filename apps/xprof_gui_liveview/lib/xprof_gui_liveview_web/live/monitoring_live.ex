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
      :timer.send_interval(2000, self(), :update_graphs)
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
     |> assign(:graph_data, %{})
     |> assign(:last_timestamps, %{})
     |> assign(:charts, %{})
     |> assign(:callees, nil)
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
          favourites_as_suggestions(socket.assigns.favourites)
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

            # Initialize graph data for newly monitored function
            new_charts =
              Enum.reduce(monitored, socket.assigns.charts, fn mon, charts ->
                if Map.has_key?(charts, mon.mfa_str) do
                  charts
                else
                  Map.put(charts, mon.mfa_str, build_chart_for_function(mon.mfa_str))
                end
              end)

            {:noreply,
             socket
             |> assign(
               query: "",
               functions: [],
               monitored_functions: monitored,
               recent_queries: updated_recent,
               history_position: -1,
               charts: new_charts
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
        updated_graph_data = Map.delete(socket.assigns.graph_data, mfa_str)
        updated_timestamps = Map.delete(socket.assigns.last_timestamps, mfa_str)
        updated_charts = Map.delete(socket.assigns.charts, mfa_str)

        {:noreply,
         socket
         |> assign(
           monitored_functions: monitored,
           graph_data: updated_graph_data,
           last_timestamps: updated_timestamps,
           charts: updated_charts
         )
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
  def handle_event("toggle_input_type", %{"type" => type}, socket) do
    new_type = String.to_existing_atom(type)
    {:noreply, assign(socket, input_type: new_type, query: "", functions: [])}
  end

  def handle_event("toggle_input_type", _params, socket) do
    # For keyboard shortcut (Ctrl+I) - toggle between modes
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
  def handle_event("switch_grid_key", %{"key" => "ArrowDown"}, socket) do
    {:noreply, assign(socket, grid: min(socket.assigns.grid + 1, 4))}
  end

  def handle_event("switch_grid_key", %{"key" => "ArrowUp"}, socket) do
    {:noreply, assign(socket, grid: max(socket.assigns.grid - 1, 1))}
  end

  def handle_event("switch_grid_key", %{"key" => key}, socket) when key in ["1", "2", "3", "4"] do
    {:noreply, assign(socket, grid: String.to_integer(key))}
  end

  def handle_event("switch_grid_key", _, socket), do: {:noreply, socket}

  @impl true
  def handle_event("monitor_callee", %{"query" => query}, socket) do
    case monitor_function(query) do
      :ok ->
        monitored = fetch_monitored_functions()
        new_charts =
          Enum.reduce(monitored, socket.assigns.charts, fn mon, charts ->
            if Map.has_key?(charts, mon.mfa_str), do: charts,
              else: Map.put(charts, mon.mfa_str, build_chart_for_function(mon.mfa_str))
          end)
        {:noreply,
         socket
         |> assign(monitored_functions: monitored, callees: nil, charts: new_charts)
         |> put_flash(:info, "Started monitoring: #{query}")}

      {:error, :already_traced} ->
        {:noreply, socket |> assign(callees: nil) |> put_flash(:info, "Already being monitored")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to monitor: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("start_capture", %{"mfa" => mfa_str} = params, socket) do
    mfa = parse_mfa(mfa_str)
    threshold = params |> Map.get("threshold", "0") |> parse_non_neg_integer(0)
    limit = params |> Map.get("limit", "100") |> parse_non_neg_integer(100)

    case start_capture(mfa, threshold, limit) do
      {:ok, _capture_id} ->
        # Immediately seed capture_data so the periodic updater starts polling
        capture_data =
          case fetch_captured_data(mfa, 0) do
            {:ok, spec, items} ->
              Map.put(socket.assigns.capture_data, mfa_str, %{spec: spec, items: items, visible: 20})
            _ ->
              socket.assigns.capture_data
          end

        {:noreply, socket |> assign(:capture_data, capture_data) |> put_flash(:info, "Started capturing calls for #{format_mfa(mfa)}")}

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

    case fetch_captured_data(mfa, 0) do
      {:ok, spec, items} ->
        existing = socket.assigns.capture_data[mfa_str]
        visible = if existing, do: existing.visible, else: 20
        capture_data = Map.put(socket.assigns.capture_data, mfa_str, %{spec: spec, items: items, visible: visible})
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

      # In favourites mode, replace query; in search mode, append
      updated_query = case socket.assigns.input_type do
        :favourites -> new_value
        :search -> socket.assigns.query <> new_value
      end

      # Regenerate autocomplete suggestions based on the updated query
      new_suggestions = case socket.assigns.input_type do
        :search ->
          if String.length(updated_query) >= 2 do
            fetch_autocomplete_functions(updated_query)
          else
            []
          end
        :favourites ->
          []  # Clear suggestions after selecting a favourite
      end

      {:noreply,
       socket
       |> assign(query: updated_query, functions: new_suggestions, position: -1)
       |> push_event("set_query", %{value: updated_query})}
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
    updated_capture_data =
      socket.assigns.capture_data
      |> Enum.map(fn {mfa_str, current} ->
        mfa = parse_mfa(mfa_str)
        case fetch_captured_data(mfa, 0) do
          {:ok, spec, items} ->
            {mfa_str, %{spec: spec, items: items, visible: current.visible}}
          {:error, _reason} ->
            {mfa_str, current}
        end
      end)
      |> Map.new()

    {:noreply, assign(socket, capture_data: updated_capture_data)}
  end

  @impl true
  def handle_event("capture_more", %{"mfa" => mfa_str}, socket) do
    capture_data = socket.assigns.capture_data

    updated =
      case Map.get(capture_data, mfa_str) do
        nil -> capture_data
        capture -> Map.put(capture_data, mfa_str, %{capture | visible: capture.visible + 20})
      end

    {:noreply, assign(socket, capture_data: updated)}
  end

  @impl true
  def handle_event("show_callees", %{"mfa" => mfa_str}, socket) do
    mfa = parse_mfa(mfa_str)

    callees =
      try do
        funs = :xprof_core.get_called_funs_pp(mfa)
        %{mfa_str: mfa_str, list: Enum.map(funs, &to_string/1)}
      rescue
        _ -> %{mfa_str: mfa_str, list: []}
      end

    {:noreply, assign(socket, callees: callees)}
  end

  @impl true
  def handle_event("hide_callees", _params, socket) do
    {:noreply, assign(socket, callees: nil)}
  end

  @impl true
  def handle_info(:update_graphs, socket) do
    # When tracing is paused xprof_core trace handlers still write zero-count ETS entries
    # every second, so timestamps keep advancing. Freeze the graph server-side instead.
    if socket.assigns.trace_status != "running" do
      {:noreply, socket}
    else
    monitored_mfas = Enum.map(socket.assigns.monitored_functions, & &1.mfa)

    {updated_graph_data, updated_timestamps, updated_charts} =
      Enum.reduce(
        monitored_mfas,
        {socket.assigns.graph_data, socket.assigns.last_timestamps, socket.assigns.charts},
        fn mfa, {graph_acc, ts_acc, chart_acc} ->
          mfa_str = serialize_mfa(mfa)
          last_ts = Map.get(ts_acc, mfa_str, 0)

          case fetch_graph_data(mfa, last_ts) do
            {:ok, new_data, new_timestamp} when map_size(new_data) > 0 ->
              existing_data =
                Map.get(graph_acc, mfa_str, %{count: [], mean: [], min: [], p99: []})

              # Maintain 2-minute rolling window (120 points at 1 sample/s)
              merged_data = %{
                count: maintain_rolling_window(existing_data.count ++ new_data.count, 120),
                mean: maintain_rolling_window(existing_data.mean ++ new_data.mean, 120),
                min: maintain_rolling_window(existing_data.min ++ new_data.min, 120),
                p99: maintain_rolling_window(existing_data.p99 ++ new_data.p99, 120)
              }

              chart = Map.get(chart_acc, mfa_str)

              updated_chart =
                if chart do
                  %{
                    chart
                    | series: [
                        %{name: "Min", data: merged_data.min},
                        %{name: "Mean", data: merged_data.mean},
                        %{name: "P99", data: merged_data.p99},
                        %{name: "Count", data: merged_data.count}
                      ]
                  }
                else
                  build_chart_for_function(mfa_str)
                end

              {
                Map.put(graph_acc, mfa_str, merged_data),
                Map.put(ts_acc, mfa_str, new_timestamp),
                Map.put(chart_acc, mfa_str, updated_chart)
              }

            _ ->
              {graph_acc, ts_acc, chart_acc}
          end
        end
      )

    {:noreply,
     assign(socket,
       graph_data: updated_graph_data,
       last_timestamps: updated_timestamps,
       charts: updated_charts
     )}
    end
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
    try do
      case :xprof_core.get_mode() do
        mode when mode in [:erlang, :elixir] -> Atom.to_string(mode)
        _ -> "unknown"
      end
    rescue
      _ -> "unknown"
    end
  end

  defp fetch_trace_status do
    try do
      case :xprof_core.get_trace_status() do
        {_, :running} -> "running"
        {_, :paused} -> "paused"
        {_, :initialized} -> "paused"
        {_, :overflow} -> "running"
        _ -> "paused"
      end
    rescue
      _ -> "paused"
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

  defp fetch_graph_data(mfa, last_timestamp) when is_tuple(mfa) do
    try do
      case :xprof_core.get_data_pp(mfa, last_timestamp) do
        {:error, :not_found} ->
          {:ok, [], last_timestamp}

        data when is_list(data) ->
          transformed_data = transform_graph_data(data)

          new_timestamp =
            case List.last(data) do
              nil -> last_timestamp
              last_item -> :proplists.get_value(:time, last_item, last_timestamp)
            end

          {:ok, transformed_data, new_timestamp}

        _ ->
          {:ok, [], last_timestamp}
      end
    rescue
      e ->
        Logger.error("Failed to fetch graph data for #{inspect(mfa)}: #{inspect(e)}")
        {:ok, [], last_timestamp}
    end
  end

  defp fetch_graph_data(_mfa, last_timestamp), do: {:ok, [], last_timestamp}

  defp transform_graph_data(data) when is_list(data) do
    # Transform Erlang proplists to ApexCharts format.
    # xprof_core reads snapshots from a `set` ETS table, so they come back in
    # unspecified order. ApexCharts connects line points in array order, so we
    # must sort by timestamp here or the line zig-zags (criss-crosses).
    data
    |> Enum.sort_by(fn snapshot -> :proplists.get_value(:time, snapshot, 0) end)
    |> Enum.reduce(%{count: [], mean: [], min: [], p99: []}, fn snapshot, acc ->
      time = :proplists.get_value(:time, snapshot, 0)
      timestamp_ms = time * 1000

      count = :proplists.get_value(:count, snapshot, 0)
      mean = :proplists.get_value(:mean, snapshot, 0)
      min = :proplists.get_value(:min, snapshot, 0)
      p99 = :proplists.get_value(:p99, snapshot, 0)

      %{
        count: acc.count ++ [%{x: timestamp_ms, y: count}],
        mean: acc.mean ++ [%{x: timestamp_ms, y: mean}],
        min: acc.min ++ [%{x: timestamp_ms, y: min}],
        p99: acc.p99 ++ [%{x: timestamp_ms, y: p99}]
      }
    end)
  end

  defp transform_graph_data(_), do: %{count: [], mean: [], min: [], p99: []}

  defp build_chart_for_function(_mfa_str) do
    %{
      chart: %{
        type: "line",
        height: 300,
        animations: %{enabled: false},
        toolbar: %{show: false}
      },
      series: [
        %{name: "Min", data: []},
        %{name: "Mean", data: []},
        %{name: "P99", data: []},
        %{name: "Count", data: []}
      ],
      stroke: %{width: 2, curve: "straight"},
      colors: ["#D3D004", "#FFAA00", "#E24806", "#98FB98"],
      xaxis: %{
        type: "datetime",
        range: 120_000,
        labels: %{format: "HH:mm:ss"}
      },
      yaxis: [
        %{
          seriesName: "Min",
          title: %{text: "Time (µs)"},
          min: 0,
          showAlways: true
        },
        %{seriesName: "Mean", show: false},
        %{seriesName: "P99", show: false},
        %{
          seriesName: "Count",
          opposite: true,
          title: %{text: "Count"},
          min: 0,
          showAlways: true
        }
      ],
      legend: %{position: "top", horizontalAlign: "left"},
      tooltip: %{shared: true, x: %{format: "HH:mm:ss"}}
    }
  end

  defp maintain_rolling_window(data_points, max_size) when is_list(data_points) do
    if length(data_points) > max_size do
      Enum.take(data_points, -max_size)
    else
      data_points
    end
  end

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

  defp favourites_as_suggestions(favourites) do
    # Wrap favourites as maps for autocomplete suggestions
    Enum.map(favourites, fn fav ->
      %{value: to_string(fav), label: to_string(fav)}
    end)
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

        {:noreply,
         socket
         |> assign(query: query, history_position: new_pos)
         |> push_event("set_query", %{value: query})}
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
        {:noreply,
         socket
         |> assign(query: query, history_position: new_pos)
         |> push_event("set_query", %{value: query})}
      else if current_pos == 0 do
        # Go back to empty query
        {:noreply,
         socket
         |> assign(query: "", history_position: -1)
         |> push_event("set_query", %{value: ""})}
      else
        {:noreply, socket}
      end
      end
    end
  end

  defp handle_key_event("Tab", socket) do
    if length(socket.assigns.functions) > 0 do
      values = Enum.map(socket.assigns.functions, fn
        %{value: val} -> val
        val when is_binary(val) -> val
        _ -> ""
      end)

      prefix = longest_common_prefix(values)

      # In search mode, suggestions are tails to append; in favourites mode they are full names
      updated_query = case socket.assigns.input_type do
        :favourites -> prefix
        :search -> socket.assigns.query <> prefix
      end

      if prefix != "" and updated_query != socket.assigns.query do
        new_suggestions = case socket.assigns.input_type do
          :search ->
            if String.length(updated_query) >= 2 do
              fetch_autocomplete_functions(updated_query)
            else
              []
            end
          :favourites ->
            filter_favourites(socket.assigns.favourites, updated_query)
        end
        {:noreply,
         socket
         |> assign(query: updated_query, functions: new_suggestions, position: -1)
         |> push_event("set_query", %{value: updated_query})}
      else
        {:noreply, socket}
      end
    else
      {:noreply, socket}
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

      # In favourites mode, replace query; in search mode, append
      updated_query = case socket.assigns.input_type do
        :favourites -> new_value
        :search -> socket.assigns.query <> new_value
      end

      # Regenerate autocomplete suggestions based on the updated query
      new_suggestions = case socket.assigns.input_type do
        :search ->
          if String.length(updated_query) >= 2 do
            fetch_autocomplete_functions(updated_query)
          else
            []
          end
        :favourites ->
          []  # Clear suggestions after selecting a favourite
      end

      {:noreply,
       socket
       |> assign(query: updated_query, functions: new_suggestions, position: -1)
       |> push_event("set_query", %{value: updated_query})}
    else
      # Let the form submit handle it
      {:noreply, socket}
    end
  end

  defp handle_key_event(_key, socket) do
    {:noreply, socket}
  end

  # Find longest common prefix of a list of strings
  defp longest_common_prefix([]), do: ""
  defp longest_common_prefix([single]), do: single
  defp longest_common_prefix(strings) do
    first = hd(strings)
    rest = tl(strings)

    String.graphemes(first)
    |> Enum.reduce_while("", fn char, acc ->
      new_prefix = acc <> char
      if Enum.all?(rest, &String.starts_with?(&1, new_prefix)) do
        {:cont, new_prefix}
      else
        {:halt, acc}
      end
    end)
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

  def get_icon_path(mode) do
    case mode do
      "erlang" -> "/images/xprof_icon_erlang.png"
      "elixir" -> "/images/xprof_icon_elixir.png"
      _ -> "/images/xprof_icon.png"
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
    "#{mod}:#{fun}/#{arity}"
  end

  defp parse_non_neg_integer(str, default) when is_binary(str) do
    case Integer.parse(str) do
      {n, ""} when n >= 0 -> n
      _ -> default
    end
  end
end
