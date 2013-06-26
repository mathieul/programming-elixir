defmodule Issues.CLI do
  def parse_args(argv) do
    parse = OptionParser.parse(argv, switches: [ help: :boolean ],
                                     aliases:  [ h:    :help    ])
    case parse do
      { [ help: true ], _ } -> :help
      _ -> :help
    end
  end

end