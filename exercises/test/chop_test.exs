Code.require_file "test_helper.exs", __DIR__

defmodule ChopTest do
  use ExUnit.Case

  doctest Chop

  test "return answer directly if range just has that one element" do
    assert Chop.guess(273, 273..273) == [{ :answer, 273 }]
  end

  test "return an empty list if the value is not in the range" do
    assert Chop.guess(42, 4..8) == []
  end

  test "return the list of guesses and the answer when the value is in range" do
    expected_result = [
      { :guess, 500 },
      { :guess, 250 },
      { :guess, 375 },
      { :guess, 312 },
      { :guess, 281 },
      { :guess, 265 },
      { :guess, 273 },
      { :answer, 273 }
    ]
    assert Chop.guess(273, 1..1000) == expected_result
  end
end
