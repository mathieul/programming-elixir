defmodule Issues.CLI do
  @default_count 4

  def run(argv) do
    parse_args(argv)
  end


  def parse_args(argv) do
    parse = OptionParser.parse(argv, switches: [ help: :boolean ],
                                     aliases:  [ h:    :help    ])
    case parse do
      { [ help: true ], _ }           -> :help
      { _, [ user, project, count ] } -> { user, project, binary_to_integer(count) }
      { _, [ user, project ] }        -> { user, project, @default_count }
      _                               -> :help
    end
  end
end
