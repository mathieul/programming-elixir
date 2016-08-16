defmodule StageExamples.Consumer do
  alias Experimental.DynamicSupervisor
  alias StageExamples.{Counter, Printer}

  @moduledoc """
  A consumer will be a dynamic supervisor that will
  spawn printer tasks for each event.
  """

  use DynamicSupervisor

  def start_link() do
    DynamicSupervisor.start_link(__MODULE__, :ok)
  end

  # Callbacks

  def init(:ok) do
    children = [
      worker(Printer, [], restart: :temporary)
    ]

    {:ok, children, strategy: :one_for_one, subscribe_to: [Counter]}
  end
end
