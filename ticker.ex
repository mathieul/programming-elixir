defmodule Tick do

  @interval 2000
  @name :ticker

  def start do
    pid = spawn(__MODULE__, :generator, [[]])
    :global.register_name(@name, pid)
  end

  def register(client_id) do
    :global.whereis_name(@name) <- { :register, client_id }
  end

  def generator(clients) do
    receive do
      { :register, pid } ->
        IO.puts "registering #{inspect pid}"
        generator([ pid | clients ])

      after @interval ->
        IO.puts "tick"
        if length(clients) > 0 do
          [ client | rest ] = clients
          client <- { :tick }
          generator(rest ++ [client])
        else
          generator([])
        end
    end
  end
end

defmodule Client do
  def start do
    pid = spawn(__MODULE__, :receiver, [])
    Tick.register(pid)
  end

  def receiver do
    receive do
      { :tick } ->
        IO.puts "tock in client #{inspect self}"
        receiver
    end
  end
end
