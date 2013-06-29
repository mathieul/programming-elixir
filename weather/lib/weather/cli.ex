defmodule Weather.CLI do

  @headers ["code", "location", "time", "weather", "temperature"]

  def run(argv) do
    argv
      |> parse_args
      |> process
  end

  def parse_args(args) do
    case OptionParser.parse(args, switches: [help: :boolean, async: :boolean], aliases: [h: :help]) do
      { [help: true], _ } -> :help
      { [async: async, help: _help], location_codes } -> { location_codes, async }
      _ -> :help
    end
  end

  def process(:help) do
    IO.puts """
    Usage: weather <location code>
    """
    System.halt(0)
  end

  def process({ location_codes, false }) do
    weather = Weather.NOAA.get_current_weather_for_locations(location_codes)
    Weather.TableFormatter.print_table_for_columns(weather, @headers)
  end

  def process({ location_codes, true }) do
    me = self
    printer = spawn_link fn ->
      column_widths = [5, 30, 20, 30, 20]
      format = Weather.TableFormatter.format_for(column_widths)
      Weather.TableFormatter.puts_one_line_in_columns(@headers, format)
      IO.puts Weather.TableFormatter.separator(column_widths)
      Enum.each location_codes, fn ->
        receive do
          { :weather, row } ->
            Weather.TableFormatter.puts_in_columns([row], @headers, format)
        end
      end
      me <- :done
    end
    Enum.each location_codes, fn code ->
      spawn_link fn ->
        (printer <- { :weather, Weather.NOAA.get_current_weather(code) })
      end
    end
    receive do _ -> IO.puts "Done." end
  end
end
