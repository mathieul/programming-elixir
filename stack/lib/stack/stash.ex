defmodule Stack.Stash do
  use GenServer.Behaviour

  #####
  # External API

  def start_link(value), do: :gen_server.start_link(__MODULE__, value, [])
  def save_value(pid, value), do: :gen_server.cast(pid, { :save_value, value })
  def get_value(pid), do: :gen_server.call(pid, :get_value)

  #####
  # GenServer implementation

  def init(list) when is_list(list), do: { :ok, list }
  def handle_call(:get_value, _from, list), do: { :reply, list, list }
  def handle_cast({ :save_value, new_list }, _list), do: { :noreply, new_list }
end