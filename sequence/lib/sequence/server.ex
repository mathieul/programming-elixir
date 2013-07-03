defmodule Sequence.Server do
  use GenServer.Behaviour

  def init(current_number)
  when is_number(current_number) do
    { :ok, current_number }
  end

  def handle_call(:next_number, _from, current_number) do
    { :reply, current_number, current_number + 1 }
  end

  # def handle_call({:set_number, new_number}, _from, current_number) do
  #   { :reply, new_number, new_number }
  # end

  def handle_cast({:set_number, new_number}, _current_number) do
    { :noreply, new_number }
  end

  def handle_cast({:inc_number, delta}, current_number) do
    { :noreply, current_number + delta }
  end

  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "My current state is '#{inspect state}', and I'm happy"}]]
  end
end
