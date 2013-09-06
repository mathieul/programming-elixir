defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    conn.fetch([:params])
  end

  post "/publish" do
    room_id = conn.params[:room_id]
    data = conn.params[:data]
    if room_id && data do
      Chaussette.Pubsub.publish(room_id, data)
      conn.resp 200, "Message sent."
    else
      conn.resp 406, "Not Acceptable"
    end
  end

  websocket "/web-socket", using: Chaussette.WebsocketHandler
end
