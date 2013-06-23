defmodule MyList do
  @doc """
  MyList.mapsum [1, 2, 3], &1 * &1
  iex> 14
  """
  def mapsum(list, summer), do: do_mapsum(list, summer, 0)

  defp do_mapsum([], _, total), do: total

  defp do_mapsum([item | list], summer, total) do
    do_mapsum(list, summer, total + summer.(item))
  end

  @doc """
  MyList.max [2, 6, 1, 14, 9]
  iex> 14
  """
  def max(list), do: do_max(list, 0)

  defp do_max([], acc), do: acc

  defp do_max([item | list], acc) do
    do_max(list, if item > acc, do: item, else: acc)
  end

  @doc """
  MyList.caesar 'HAL', 1
  iex> IBM
  """
  def caesar(list, translation), do: do_caesar(list, translation, '')

  defp do_caesar([], _, encoded), do: Enum.reverse(encoded)
  defp do_caesar([item | list], translation, encoding) do
    result = item + translation
    if result > 122, do: result = 63
    do_caesar(list, translation, [result | encoding])
  end

  @doc """
  MyList.flatten([ 1, [ 2, 3, [4] ], 5, [[[6]]]])
  iex> [1,2,3,4,5,6]
  """
  def flatten(list), do: Enum.reverse do_flatten(list, [])

  def do_flatten([], flat), do: flat

  def do_flatten([item | list], flat) do
    if is_list(item) do
      do_flatten(list, do_flatten(item, []) ++ flat)
    else
      do_flatten(list, [item | flat])
    end
  end
end
