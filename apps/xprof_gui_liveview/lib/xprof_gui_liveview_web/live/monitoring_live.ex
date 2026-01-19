defmodule XprofGuiLiveviewWeb.MonitoringLive do
  use XprofGuiLiveviewWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Start periodic updates when LiveView connects
      :timer.send_interval(1000, self(), :update_status)
      :timer.send_interval(5000, self(), :update_functions)
      :timer.send_interval(30000, self(), :update_favourites)
    end

    {:ok,
     socket
     |> assign(:query, "")
     |> assign(:functions, [])
     |> assign(:monitored_functions, [])
     |> assign(:position, -1)
     |> assign(:input_type, :search)
     |> assign(:trace_status, "paused")
     |> assign(:mode, nil)
     |> assign(:grid, 1)
     |> assign(:favourites, [])
     |> fetch_initial_data()}
  end

  @impl true
  def handle_event("update_query", %{"query" => query}, socket) do
    # Handle query input changes
    functions = if String.length(query) >= 2 do
      fetch_autocomplete_functions(query)
    else
      []
    end

    {:noreply, assign(socket, query: query, functions: functions, position: -1)}
  end

  @impl true
  def handle_event("submit_query", %{"query" => query}, socket) do
    # Start monitoring the function
    case monitor_function(query) do
      {:ok, _} ->
        monitored = fetch_monitored_functions()
        {:noreply, assign(socket, query: "", functions: [], monitored_functions: monitored)}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to monitor: #{inspect(reason)}")}
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
  def handle_event("select_function", %{"index" => index_str}, socket) do
    index = String.to_integer(index_str)
    functions = socket.assigns.functions

    if index >= 0 and index < length(functions) do
      function = Enum.at(functions, index)
      {:noreply, assign(socket, query: function, position: index)}
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
    {:noreply, assign(socket, monitored_functions: monitored)}
  end

  @impl true
  def handle_info(:update_favourites, socket) do
    favourites = fetch_favourites()
    {:noreply, assign(socket, favourites: favourites)}
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
      _ -> "unknown"
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
      _ -> "paused"
    end
  end

  defp fetch_monitored_functions do
    # Get list of monitored functions from xprof_core
    try do
      monitored = :xprof_core.get_all_monitored()
      Enum.map(monitored, &format_monitored_function/1)
    rescue
      _ -> []
    end
  end

  defp fetch_favourites do
    # TODO: Get favourites list from xprof_gui_favourites
    # For now, return empty list since we don't have xprof_gui as dependency
    []
  end

  defp fetch_autocomplete_functions(_query) do
    # TODO: Implement autocomplete using xprof_core API
    # For now return empty list
    []
  end

  defp monitor_function(query) do
    # Start monitoring a function
    try do
      :xprof_core.monitor(String.to_charlist(query))
    rescue
      e -> {:error, e}
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
      _ -> "paused"
    end
  end

  defp format_monitored_function(fun_data) do
    # Format the monitored function data for display
    # fun_data is from xprof_core, convert to map
    %{
      mfa: fun_data[:mfa] || [],
      query: fun_data[:query] || "",
      graph_type: fun_data[:graph_type] || "default"
    }
  end

  defp handle_key_event("ArrowUp", socket) do
    # Move up in autocomplete list
    new_position = max(socket.assigns.position - 1, -1)
    {:noreply, assign(socket, position: new_position)}
  end

  defp handle_key_event("ArrowDown", socket) do
    # Move down in autocomplete list
    max_pos = length(socket.assigns.functions) - 1
    new_position = min(socket.assigns.position + 1, max_pos)
    {:noreply, assign(socket, position: new_position)}
  end

  defp handle_key_event("Enter", socket) do
    # Submit query or select from autocomplete
    if socket.assigns.position >= 0 do
      # Select highlighted function
      function = Enum.at(socket.assigns.functions, socket.assigns.position)
      handle_event("submit_query", %{"query" => function}, socket)
    else
      # Submit current query
      handle_event("submit_query", %{"query" => socket.assigns.query}, socket)
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

  def format_mfa(mfa) when is_list(mfa) and length(mfa) == 3 do
    [mod, fun, arity] = mfa
    "#{mod}:#{fun}/#{arity}"
  end

  def format_mfa(_), do: "unknown"
end
