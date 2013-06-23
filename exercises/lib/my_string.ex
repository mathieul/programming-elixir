defmodule MyString do
  @doc """
  Function is_printable?
  iex> MyString.is_printable? 'allo blah ~'
  true
  iex> MyString.is_printable? 'accéder'
  false
  """
  def is_printable?(list) when is_list(list) do
    Enum.all?(list, fn char -> char > 31 && char <= ?~ end)
  end

  @doc """
  Function anagram?
  iex> MyString.anagram? 'anagram', 'margana'
  true
  iex> MyString.anagram? 'allo', 'ola'
  false
  """
  def anagram?(word1, word2), do: word1 == Enum.reverse(word2)

  @doc """
  Function calculate
  iex> MyString.calculate("123 + 27")
  150
  """
  def calculate(expression), do: 0
end
