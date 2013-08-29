defmodule Chaussette.PubsubSupervisor do
  use Supervisor.Behaviour

  #####
  # External API

  def start_link(_) do
    :supervisor.start_link(__MODULE__, nil)
  end

  #####
  # Supervisor implementation

  def init(_) do
    supervise([ worker(Chaussette.Pubsub, [ nil ]) ], strategy: :one_for_one)
  end
end
