defmodule Docs.Authentication do
  import Plug.Conn

  def init(opts),
    do: PlugBasicAuth.init(opts)

  def call(conn, opts) do
    case PlugBasicAuth.call(conn, opts) do
      %Plug.Conn{halted: true} = conn ->
        conn
      conn ->
        token = Phoenix.Token.sign(conn, "user token", "ze user id")
        conn
        |> assign(:current_user, true)
        |> assign(:user_token, token)
        |> put_session(:user_id, 42)
    end
  end
end
