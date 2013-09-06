defmodule Chaussette.WebsocketHandler do
  @behaviour :cowboy_websocket_handler

  alias Chaussette.Pubsub

  def websocket_init(_transport_name, req, [ conn: conn ]) do
    room_id = conn.params[:room_id]
    if room_id, do: Pubsub.register(room_id, self)
    { :ok, req, [ room: room_id ] }
  end

  def websocket_handle( { :text, message }, req, state) do
    { :reply, {:text, message }, req, state }
  end
  def websocket_handle(_data, req, state), do: { :ok, req, state}

  def websocket_info({ :pubsub_message, message }, req, state) do
    { :reply, { :text, message }, req, state }
  end
  def websocket_info(_data, req, state), do: { :ok, req, state }

  def websocket_terminate(_reason, _req, state) do
    [ room: room ] = state
    Pubsub.unregister(room, self)
    :ok
  end
end
