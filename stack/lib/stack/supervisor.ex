defmodule Stack.Supervisor do
  use Supervisor.Behaviour

  #####
  # External API

  def start_link(initial_list) do
    result = { :ok, sup } = :supervisor.start_link(__MODULE__, [])
    start_workers(sup, initial_list)
    result
  end

  def start_workers(sup, initial_list) do
    { :ok, stash } = :supervisor.start_child(sup, worker(Stack.Stash, [ initial_list ]))
    :supervisor.start_child(sup, supervisor(Stack.SubSupervisor, [ stash ]))
  end

  #####
  # Supervisor implementation

  def init(_) do
    supervise([], strategy: :one_for_one)
  end
end
