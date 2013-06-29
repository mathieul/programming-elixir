defmodule Weather.CLI do
  def run(argv) do
    argv
      |> parse_args
      |> process
  end


  def parse_args(args) do
    case OptionParser.parse(args, switches: [help: :boolean], aliases: [h: :help]) do
      { [help: true], _ }     -> :help
      { _, [location_code] }  -> location_code
      _                       -> :help
    end
  end

  def process(:help) do
    IO.puts """
    Usage: weather <location code>
    """
    System.halt(0)
  end

  def process(location_code) do
    weather = Weather.NOAA.get_current_weather(location_code)
    Weather.TableFormatter.print_table_for_columns([weather],
      ["code", "location", "time", "weather", "temperature"])
  end
end
