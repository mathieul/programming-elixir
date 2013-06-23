defmodule Chop do
  @doc """
  Guess a value from a range until the answer.

      iex> Chop.guess(42, 1..100)
      [guess: 50, guess: 25, guess: 37, guess: 43, guess: 40, guess: 41, guess: 42, answer: 42]
  """
  def guess(value, from..to) when ! value in from..to, do: []

  def guess(_, from..to) when from == to, do: [{ :answer, from }]

  def guess(value, range) do
    guesses = do_guess(value, range, [])
    { :guess, found } = List.last(guesses)
    guesses ++ [{:answer, found}]
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
