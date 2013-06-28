defmodule Weather.Mixfile do
  use Mix.Project

  def project do
    [ app: :weather,
      version: "0.0.1",
      name: "Weather",
      source_url: "https://github.com/mathieul/programming-elixir",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [ :httpotion ] ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :httpotion, "0.1.0", [ github: "myfreeweb/httpotion" ] }
    ]
  end
end
