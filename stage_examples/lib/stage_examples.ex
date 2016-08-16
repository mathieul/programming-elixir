defmodule StageExamples do
  use Application
  alias Experimental.GenStage
  alias StageExamples.{A, B, C}
  alias StageExamples.{Counter, Consumer}

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(Counter, [0]),
      # We can add as many dynamic supervisors as consumers as we want!
      worker(Consumer, [], id: 1)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def producer_consumer do
    with {:ok, a} <- GenStage.start_link(A, 0),
         {:ok, b} <- GenStage.start_link(B, 2),
         {:ok, c} <- GenStage.start_link(C, :ok) do

      GenStage.sync_subscribe(b, to: a)
      GenStage.sync_subscribe(c, to: b)
    end
  end
end
