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

  def handle_cast({ :push, element }, current_list) do
    { :noreply, [ element, current_list ]}
  end

  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "full list content: #{inspect state}"}]]
  end
end
