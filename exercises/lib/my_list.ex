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
end
