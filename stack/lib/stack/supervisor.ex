defmodule Stack.Supervisor do
  use Supervisor.Behaviour

  #####
  # External API

  def start_link(initial) do
    result = { :ok, sup } = :supervisor.start_link(__MODULE__, [])
    start_workers(sup, initial)
    result
  end

  def start_workers(sup, initial) do
    { :ok, stash } = :supervisor.start_child(sup, worker(Stack.Stash, [ initial ]))
    :supervisor.start_child(sup, supervisor(Stack.SubSupervisor, [ stash ]))
  end

  #####
  # Supervisor implementation

  def init(_) do
    supervise([], strategy: :one_for_one)
  end
end
