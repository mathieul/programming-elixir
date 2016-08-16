defmodule StageExamples.Counter do
  alias Experimental.GenStage

  @moduledoc """
  This is a simple producer that counts from the given
  number whenever there is a demand.
  """

  use GenStage

  def start_link(initial) when is_integer(initial) do
    GenStage.start_link(__MODULE__, initial, name: __MODULE__)
  end

  ## Callbacks

  def init(initial) do
    {:producer, initial}
  end

  def handle_demand(demand, counter) when demand > 0 do
    # If the counter is 3 and we ask for 2 items, we will
    # emit the items 3 and 4, and set the state to 5.
    events = Enum.to_list(counter..counter+demand-1)
    {:noreply, events, counter + demand}
  end
end
