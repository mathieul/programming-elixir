defmodule Chaussette.WebsocketClient do
  @behaviour :websocket_client_handler

  def start_link do
    :websocket_client.start_link('ws://localhost:8889/web-socket?room_id=client', __MODULE__, [12])
  end

  def init([num], _conn_state) do
    IO.puts "init(#{inspect num})"
    :websocket_client.cast(self, { :text, "message 1" })
    { :ok, 2 }
  end

  def websocket_handle({ :pong, _ }, _conn_state, state) do
    { :ok, state }
  end

  def websocket_handle({ :text, message }, _conn_state, state) do
    IO.puts ">>> message received: #{inspect message}"
    { :ok, state }
    # { :reply, { :text, "hello there" }, state}
  end
end
