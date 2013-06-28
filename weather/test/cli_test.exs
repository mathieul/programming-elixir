Code.require_file "test_helper.exs", __DIR__

defmodule CliTest do
  use ExUnit.Case

  import Weather.CLI, only: [parse_args: 1]

  test "#parse_args with --help or -h returns :help" do
    assert parse_args(["--help"]) == :help
    assert parse_args(["-h"]) == :help
  end

  test "#parse_args with location code returns it" do
    assert parse_args(["KSQL"]) == "KSQL"
  end
end
