defmodule Stack do
  def start(_type, initial) do
    Stack.Supervisor.start_link(initial)
  end
end
