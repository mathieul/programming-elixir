defmodule My do
  defmacro macro(param) do
    IO.puts "param: #{inspect param}"
  end
end

defmodule Test do
  require My

  My.macro 42
  My.macro 1.2
  My.macro [1, 2, 3]
  My.macro [debug: true, option: :blah]
  My.macro [{:ici, "Londres"}, {:allo, "La Terre???"}]
  My.macro "binaries"
  My.macro {:at, :om}
  My.macro do: 1
  My.macro 5..9
  My.macro {1, 2, 3, 4, 5}
  My.macro do: ( a = 1; a + a )
  My.macro do
    1 + 2
  else
    3 + 4
  end
end