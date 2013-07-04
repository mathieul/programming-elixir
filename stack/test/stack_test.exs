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

  test "it can return the last value popped" do
    Stack.Server.push(42)
    assert Stack.Server.pop == 42
    assert Stack.Server.last == 42
    Stack.Server.push(:go)
    assert Stack.Server.last == 42
    assert Stack.Server.pop == :go
    assert Stack.Server.last == :go
  end
end
