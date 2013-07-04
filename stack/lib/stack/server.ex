defmodule Stack.Server do
  use GenServer.Behaviour

  @vsn "1"

  #####
  # External API

  def start_link(stash_pid) do
    :gen_server.start_link({ :local, :stack }, __MODULE__, stash_pid, [])
  end

  def push(item), do: :gen_server.cast(:stack, { :push, item })
  def pop, do: :gen_server.call(:stack, :pop)
  def last, do: :gen_server.call(:stack, :last)
  def kill, do: :gen_server.cast(:stack, :die)

  #####
  # GenServer implementation

  def init(stash_pid) do
    state = Stack.Stash.get_value(stash_pid)
    { :ok, {state, stash_pid} }
  end

  def handle_call(:pop, _from, { { current_list, last }, stash_pid }) do
    if length(current_list) == 0 do
      IO.puts "stack: list is empty, exiting..."
      { :stop, { :empty_list, [] }, { { current_list, last }, stash_pid } }
    else
      [ popped | remaining ] = current_list
      { :reply, popped, { { remaining, popped }, stash_pid } }
    end
  end

  def handle_call(:last, _from, state = { { _list, last }, _stash_pid }) do
    { :reply, last, state }
  end

  def handle_cast({ :push, element }, { { current_list, last }, stash_pid}) do
    { :noreply, { { [ element | current_list ], last }, stash_pid } }
  end

  def handle_cast(:die, state), do: { :stop, :killed, state }

  def handle_info(:inspect, state) do
    IO.puts "full list content: #{inspect state}"
    { :noreply, state }
  end

  def terminate(reason, { state, stash_pid }) do
    Stack.Stash.save_value(stash_pid, state)
    IO.puts "stack server is terminating (#{inspect reason})"
  end

  def format_status(_reason, [ _pdict, state ]) do
    [ data: [ { 'State', "current list and last popped: #{inspect state}" } ] ]
  end

  def code_change("0", old_state, _extra) do
    IO.puts "Changing code from 0 to #{@vsn}"
    { :ok, { old_state, nil } }
  end
end
