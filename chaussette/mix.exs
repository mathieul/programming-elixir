defmodule Chaussette.Mixfile do
  use Mix.Project

  def project do
    [ app: :chaussette,
      version: "0.0.1",
      dynamos: [Chaussette.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      env: [prod: [compile_path: "ebin"]],
      compile_path: "tmp/#{Mix.env}/chaussette/ebin",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo],
      mod: { Chaussette, [] },
      registered: [ :pubsub ] ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :dynamo, path: "../../../Vendor/dynamo" } ]
  end
end
