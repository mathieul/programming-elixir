defmodule Stack.Server do
  use GenServer.Behaviour

  #####
  # External API

  def start_link(current_list) do
    :gen_server.start_link({ :local, :stack }, __MODULE__, current_list, [])
  end

  def push(item) do
    :gen_server.cast(:stack, { :push, item })
  end

  def pop do
    :gen_server.call(:stack, :pop)
  end

  #####
  # GenServer implementation

  def init(current_list)
  when is_list(current_list) do
    { :ok, current_list }
  end

  def handle_call(:pop, _from, current_list) do
    if length(current_list) == 0 do
      IO.puts "stack: list is empty, exiting..."
      { :stop, { :empty_list, [] }, current_list }
    else
      [ popped | remaining ] = current_list
      { :reply, popped, remaining }
    end
  end

  def handle_cast({ :push, element }, current_list) do
    { :noreply, [ element | current_list ] }
  end

  def handle_info(:inspect, state) do
    IO.puts "full list content: #{inspect state}"
    { :noreply, state }
  end

  def terminate(reason, _state) do
    IO.puts "stack server is terminating (#{inspect reason})"
  end

  def format_status(_reason, [ _pdict, state ]) do
    [ data: [ { 'State', "full list content: #{inspect state}" } ] ]
  end
end
