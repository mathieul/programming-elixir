# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :docs,
  ecto_repos: [Docs.Repo]

# Configures the endpoint
config :docs, Docs.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "lt6ug37Ucj2QfqtwVJqacwnjYVbKFA7icRxDvgFU/4YS1OWSRkR6x/r6hGP7cFlE",
  render_errors: [view: Docs.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Docs.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
