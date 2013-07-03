defmodule Stack do
  def start(_type, initial_list) do
    Stack.Supervisor.start_link(initial_list)
  end
end
