defmodule MyList do
  @doc """
  Function all?
  iex> MyList.all? [2, 4, 6, 8], fn item -> rem(item, 2) == 0 end
  true
  iex> MyList.all? [2, 3, 4, 6], fn item -> rem(item, 2) == 0 end
  false
  """
  def all?(list, tester), do: check_all?(list, tester, true)

  defp check_all?(_, _, false), do: false
  defp check_all?([], _, true), do: true
  defp check_all?([item | list], tester, true) do
    check_all?(list, tester, tester.(item))
  end

  @doc """
  Function each
  iex> MyList.each [1, 2, 3], fn(e) -> Process.put(:test_each, e * 2) end
  :ok
  iex> Process.get(:test_each)
  6
  """
  def each([], _), do: :ok
  def each([item | list], callback) do
    callback.(item)
    each(list, callback)
  end

  @doc """
  Function filter
  iex> MyList.filter [:allo, :la, :terre], fn(item) -> Regex.match? %r{a}, atom_to_binary(item) end
  [:allo, :la]
  """
  def filter(list, tester), do: do_filter(list, tester, [])

  defp do_filter([], _, acc), do: Enum.reverse(acc)
  defp do_filter([item | list], tester, acc) do
    if tester.(item) do
      do_filter(list, tester, [item | acc])
    else
      do_filter(list, tester, acc)
    end
  end

  @doc """
  Function split
  iex> MyList.split [:allo, :la, :terre, :ici, :londres], 3
  { [:allo, :la, :terre], [:ici, :londres] }
  """
  def split(list, amount), do: do_split(list, amount, [], [])

  defp do_split([], _, first, second) do
    { Enum.reverse(first), Enum.reverse(second) }
  end

  defp do_split([item | list], amount, first, second) do
    if length(first) < amount do
      do_split(list, amount, [item | first], second)
    else
      do_split(list, amount, first, [item | second])
    end
  end

  @doc """
  Function take
  iex> MyList.take [:allo, :la, :terre, :ici, :londres], 3
  [:allo, :la, :terre]
  iex> MyList.take [:un, :deux, :trois], 5
  [:un, :deux, :trois]
  """
  def take(list, amount), do: do_take(list, amount, [])

  defp do_take([], _, acc), do: Enum.reverse(acc)
  defp do_take(_, number, acc) when length(acc) == number, do: Enum.reverse(acc)
  defp do_take([item | list], number, acc), do: do_take(list, number, [item | acc])

  @doc """
  Function span
  iex> MyList.span 39, 42
  [39, 40, 41, 42]
  """
  def span(from, to), do: do_span(from, to, [])
  defp do_span(from, to, result) when from > to, do: Enum.reverse(result)
  defp do_span(from, to, result), do: do_span(from + 1, to, [from | result])

  @doc """
  Function primes
  iex> MyList.primes 20
  [2, 3, 5, 7, 11, 13, 17, 19]
  """
  def primes(n) when n >= 2 do
    lc number inlist span(2, n), is_prime?(number), do: number
  end

  defp is_prime?(2), do: true
  defp is_prime?(n) do
    [] == lc by inlist span(2, n - 1), rem(n, by) == 0, do: by
  end
end
