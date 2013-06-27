defmodule Issues.CLI do
  @default_count 4

  def run(argv) do
    argv
      |> parse_args
      |> process
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

  def process(:help) do
    IO.puts """
    Usage: issues <user> <project> [ count / #{@default_count} ]
    """
    System.halt(0)
  end

  def process({user, project, count}) do
    Issues.GithubIssues.fetch(user, project)
  end

end
