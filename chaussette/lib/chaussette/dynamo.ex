defmodule Chaussette.Dynamo do
  use Dynamo

  config :dynamo,
    # The environment this Dynamo runs on
    env: Mix.env,

    # The OTP application associated to this Dynamo
    otp_app: :chaussette,

    # The endpoint to dispatch requests to
    endpoint: ApplicationRouter,

    # The route from where static assets are served
    # You can turn off static assets by setting it to false
    static_route: "/static"

  # Uncomment the lines below to enable the cookie session store
  # config :dynamo,
  #   session_store: Session.CookieStore,
  #   session_options:
  #     [ key: "_chaussette_session",
  #       secret: "0LfCjfJXImNcl7L3xQEuW/bSEiOAyRXZuM3WaeFRmMu/Qsyjhu6UMLCKVEQ++8w1"]

  # Default functionality available in templates
  templates do
    use Dynamo.Helpers
  end
end
