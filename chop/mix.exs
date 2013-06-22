defmodule Chop.Mixfile do
  use Mix.Project

  def project do
    [ app: :chop,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    # [{:exspec, "0.1", git: "https://github.com/ekosz/exspec.git"}]
    []
  end
end
