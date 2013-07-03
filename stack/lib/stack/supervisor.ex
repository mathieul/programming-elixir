defmodule Stack.Supervisor do
  use Supervisor.Behaviour

  #####
  # External API

  def start_link(initial_list) do
    :supervisor.start_link(__MODULE__, initial_list)
  end

  #####
  # Supervisor implementation

  def init(initial_list) do
    child_processes = [ worker(Stack.Server, [initial_list]) ]
    supervise(child_processes, strategy: :one_for_one)
  end
end
