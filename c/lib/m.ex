defmodule M do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(_) do
    mem = %{}
    {:ok, mem}
  end

  def save(location, data) do
    GenServer.cast(__MODULE__, {:save, location, data})
  end

  def handle_cast({:save, location, data}, state) do
    {:noreply, Map.put(state, location, data)}
  end


end
