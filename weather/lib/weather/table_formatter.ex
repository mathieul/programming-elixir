defmodule Weather.TableFormatter do
  @doc """
  Takes a list of row data, where each row is a HashDict, and a list of headers.
  Prints a table to STDOUT of the data from each row identified by each header.
  That is, rach header identifies a column, and those columns are extracted and printed from the rows.
  We calculate the width of each column to fit the longest element in that column.
  """
  def print_table_for_columns(rows, headers) do
    data_by_columns = split_into_columns(rows, headers)
    column_widths   = widths_of(data_by_columns)
    format          = format_for(column_widths)

    puts_one_line_in_columns headers, format
    IO.puts                  separator(column_widths)
    puts_in_columns          rows, headers, format
  end

  @doc """
  Given a list or rows, where each row contains a keyed list of columns,
  return a list containing lists of the data in each column.
  The `headers` parameter contains the list of columns to extract.

  ## Example
      iex> list = [ [ {"a", "1"}, {"b", "2"}, {"c", "3"}],
      ...>          [ {"a", "4"}, {"b", "5"}, {"c", "6"}] ]
      iex> Weather.TableFormatter.split_into_columns(list, [ "a", "b", "c" ])
      [ ["1", "4"], ["2", "5"], ["3", "6"] ]
  """
  def split_into_columns(rows, headers) do
    lc header inlist headers do
      lc row inlist rows, do: printable(row[header])
    end
  end

  @doc """
  Return a binary (string) version of our parameter.

  ## Examples
      iex> Weather.TableFormatter.printable("a")
      "a"
      iex> Weather.TableFormatter.printable(99)
      "99"
  """
  def printable(str) when is_binary(str), do: str
  def printable(str), do: to_binary(str)

  @doc """
  Given a list containing sublists, where each sublist contains the data for a column,
  return a list containing the maximum width of each column

  ## Example
      iex> data = [ [ "cat", "wombat", "elk"], ["mongoose", "ant", "gnu"]]
      iex> Weather.TableFormatter.widths_of(data)
      [ 6, 8 ]
  """
  def widths_of(columns) do
    max_width = fn field, max_length -> max(String.length(field), max_length) end
    lc column inlist columns do
      Enum.reduce column, 0, max_width
    end
  end

  @doc """
  Return a format string that hard codes the widths of a set of columns.
  We put `" | "` between each column.

  ## Example
        iex> widths = [5,6,99]
        iex> Weather.TableFormatter.format_for(widths)
        "~-5s | ~-6s | ~-99s~n"
  """
  def format_for(column_widths) do
    Enum.map_join(column_widths, " | ", fn width -> "~-#{width}s" end) <> "~n"
  end

  @doc """
  Generate the line that goes below the column headings.
  It is a string of hyphens, with + signs where the verticak bar between the columns goes.

  ## Example
        iex> widths = [5,6,9]
        iex> Weather.TableFormatter.separator(widths)
        "------+--------+----------"
  """
  def separator(column_widths) do
    Enum.map_join(column_widths, "-+-", fn width -> List.duplicate("-", width) end)
  end

  @doc """
  Given a list containing rows of data, a list containing the header selectors,
  and a format string, write the extracted data under control of the format string.
  """
  def puts_in_columns([], _headers, _format), do: []
  def puts_in_columns([row|tail], headers, format) do
    extract = lc header inlist headers, do: printable(row[header])
    puts_one_line_in_columns extract, format
    puts_in_columns tail, headers, format
  end

  def puts_one_line_in_columns(fields, format) do
    :io.format(format, fields)
  end
end
