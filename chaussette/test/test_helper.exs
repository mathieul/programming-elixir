Dynamo.under_test(Chaussette.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule Chaussette.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
