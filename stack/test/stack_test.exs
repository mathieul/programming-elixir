Code.require_file "test_helper.exs", __DIR__

defmodule StackTest do
  use ExUnit.Case

  test "it can push and pop" do
    Stack.Server.push(:hello)
    Stack.Server.push("the")
    Stack.Server.push('earth')
    assert Stack.Server.pop == 'earth'
    assert Stack.Server.pop == "the"
    assert Stack.Server.pop == :hello
  end
end
