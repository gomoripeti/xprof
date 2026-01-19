defmodule XprofGuiLiveview.FavouritesStore do
  @moduledoc """
  Simple ETS-based storage for favourite queries.
  Persists favourites in memory across LiveView sessions.
  """

  use GenServer
  require Logger

  @table_name :xprof_favourites

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Add a query to favourites"
  def add(query) when is_binary(query) and byte_size(query) > 0 do
    GenServer.call(__MODULE__, {:add, query})
  end

  @doc "Remove a query from favourites"
  def remove(query) when is_binary(query) do
    GenServer.call(__MODULE__, {:remove, query})
  end

  @doc "Get all favourites"
  def list do
    GenServer.call(__MODULE__, :list)
  end

  @doc "Check if a query is in favourites"
  def member?(query) when is_binary(query) do
    GenServer.call(__MODULE__, {:member, query})
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for favourites
    # Set type: ordered_set to maintain insertion order
    # Public read access for efficiency
    table = :ets.new(@table_name, [:ordered_set, :public, :named_table])

    Logger.info("FavouritesStore started with ETS table #{@table_name}")

    {:ok, %{table: table}}
  end

  @impl true
  def handle_call({:add, query}, _from, state) do
    # Remove existing entry for this query if it exists (to update timestamp)
    case :ets.match(@table_name, {:"$1", query}) do
      [[old_timestamp] | _] ->
        :ets.delete(@table_name, old_timestamp)

      [] ->
        :ok
    end

    # Use timestamp as key to maintain order (most recent first)
    # Store as {-timestamp, query} so newest entries come first when sorted
    # Use microseconds for better resolution
    timestamp = -:erlang.system_time(:microsecond)
    :ets.insert(@table_name, {timestamp, query})

    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:remove, query}, _from, state) do
    # Find and delete the entry with this query
    # Need to scan table since query is not the key
    case :ets.match(@table_name, {:"$1", query}) do
      [[timestamp] | _] ->
        :ets.delete(@table_name, timestamp)
        {:reply, :ok, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call(:list, _from, state) do
    # Return all queries, newest first
    favourites =
      @table_name
      |> :ets.tab2list()
      |> Enum.map(fn {_timestamp, query} -> query end)

    {:reply, favourites, state}
  end

  @impl true
  def handle_call({:member, query}, _from, state) do
    result =
      case :ets.match(@table_name, {:"$1", query}) do
        [_ | _] -> true
        [] -> false
      end

    {:reply, result, state}
  end
end
