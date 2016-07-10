defmodule Docs.DocumentChannel do
  use Docs.Web, :channel

  def join("documents:" <> document_id, _params, socket) do
    {:ok, assign(socket, :document_id, document_id)}
  end

  def handle_in("text_change", %{"delta" => delta}, socket) do
    broadcast_from! socket, "text_change", %{delta: delta}
    IO.puts "broadcasted: #{inspect %{delta: delta}}"
    {:reply, :ok, socket}
  end
end
