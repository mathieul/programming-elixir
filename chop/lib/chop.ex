defmodule Chop do
  def guess(value, from..to) when ! value in from..to do
    []
  end

  def guess(value, range) do
    guesses = do_guess(value, range, [])
    { :guess, found } = List.last(guesses)
    guesses ++ [{:answer, found}]
  end

  defp do_guess(_, from..to, guesses) when from == to do
    Enum.reverse(guesses) ++ [{ :guess, from }]
  end

  defp do_guess(value, from.._, guesses) when from == value do
    Enum.reverse(guesses)
  end

  defp do_guess(value, from..to, guesses) do
    guess = from + div(to - from, 2)
    range = if guess <= value, do: guess..to, else: from..guess
    do_guess(value, range, [{ :guess, guess} | guesses])
  end
end
