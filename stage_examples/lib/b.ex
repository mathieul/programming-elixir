defmodule StageExamples.B do
  alias Experimental.{GenStage}
  use GenStage

  def init(number) do
    {:producer_consumer, number}
  end

  def handle_events(events, _from, number) do
    # If we receive [0, 1, 2], this will transform
    # it into [0, 1, 2, 1, 2, 3, 2, 3, 4].
    IO.puts "B.handle_events(#{inspect events})"
    events =
      for event <- events,
          entry <- event..event+number,
          do: entry
    {:noreply, events, number}
  end
end
