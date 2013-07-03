defmodule Stack.Server do
  use GenServer.Behaviour

  def init(current_list)
  when is_list(current_list) do
    { :ok, current_list }
  end

  def handle_call(:pop, _from, current_list) do
    [ popped | remaining ] = current_list
    { :reply, popped, remaining }
  end
end
