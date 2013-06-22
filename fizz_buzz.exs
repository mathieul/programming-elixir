fizz_buzz = function do
  {0, 0, _} -> "FizzBuzz"
  {0, _, _} -> "Fizz"
  {_, 0, _} -> "Buzz"
  {_, _, a} -> a
end

# IO.puts fizz_buzz.({0, 0, 0})
# IO.puts fizz_buzz.({0, 2, 3})
# IO.puts fizz_buzz.({2, 0, 1})
# IO.puts fizz_buzz.({9, 8, 7})

runner = fn n -> fizz_buzz.({rem(n, 3), rem(n, 5), n}) end

IO.puts runner.(10)
IO.puts runner.(11)
IO.puts runner.(12)
IO.puts runner.(13)
IO.puts runner.(14)
IO.puts runner.(15)
IO.puts runner.(16)
