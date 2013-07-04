defmodule Times do
  defmacro times_n(n) do
    # TODO
  end
end

defmodule Test do
  require Times
  Times.times_n(3)
  Times.times_n(4)
end

IO.puts "3 * 4 = #{Test.times_3(4)}"
IO.puts "4 * 5 = #{Test.times_4(5)}""
