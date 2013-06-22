defmodule Chop do
  def guess(actual, from..to) do
    guessing(div(to - from, 2), from..to, actual)
  end

  def guessing(found, _, _, found), do: IO.puts found
  def guessing(166, _, _, _), do: IO.puts "failed..."
  def guessing(maybe, from..to, actual) when maybe > actual, do: ask(maybe, from..maybe, actual)
  def guessing(maybe, from..to, actual) when maybe < actual, do: ask(maybe, maybe..to, actual)

  def ask(maybe, from..to, actual) do
    IO.puts "Is it #{maybe}?"
    guessing(div(to - from, 2), from..to, actual)
  end
end


# defmodule Chop do
#   def guess(actual, from..to) do
#     guessing(div(to - from, 2), from..to, actual)
#   end

#   def guessing(found, _, _, found), do: IO.puts found
#   def guessing(maybe, from.._, actual) when maybe > actual, do: ask(maybe, from..maybe, actual)
#   def guessing(maybe, _..to, actual) when maybe < actual, do: ask(maybe, maybe..to, actual)

#   def ask(maybe, from..to, actual) do
#     IO.puts "Is it #{maybe}?"
#     guessing(div(to - from, 2), from..to, actual)
#   end
# end

# c("chop.exs")
# Chop.guess(273, 0..1000)
