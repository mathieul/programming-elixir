defmodule My do
  defmacro if(condition, clauses) do
    do_clause = Keyword.get(clauses, :do, nil)
    IO.inspect do_clause
    else_clause = Keyword.get(clauses, :else, nil)
    quote do
      case unquote(condition) do
        _ in [false, nil] -> unquote(else_clause)
        _                 -> unquote(do_clause)
      end
    end
  end
end

defmodule Test do
  require My

  My.if 2 == 2 do
    IO.puts "2 == 2"
  else
   IO.puts "2 != 2"
  end
end
