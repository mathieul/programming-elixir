defmodule Chaussette.WebsocketHandler do
  @behaviour :cowboy_websocket_handler

  alias Chaussette.Pubsub

  def init({:tcp, :http}, req, _opts) do
    { :upgrade, :protocol, :cowboy_websocket, req, fetch_params(req) }
  end

  defp fetch_params(req) do
    { query_string, req } = :cowboy_req.qs(req)
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, _req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    params
  end

  def websocket_init(_transport_name, req, params) do
    if (room_id = params[:room_id]) do
      Pubsub.register(room_id, self)
      { :ok, req, params }
    else
      { :shutdown, req }
    end
  end

  def websocket_handle( { :text, message }, req, state) do
    { :reply, {:text, message }, req, state }
  end
  def websocket_handle(_data, req, state), do: { :ok, req, state}

  def websocket_info({ :pubsub_message, message }, req, state) do
    { :reply, { :text, message }, req, state }
  end
  def websocket_info(_data, req, state), do: { :ok, req, state }

  def websocket_terminate(_reason, _req, params) do
    Pubsub.unregister(params[:room_id], self)
    :ok
  end
end
