defmodule Docs.DocumentChannel do
  use Docs.Web, :channel

  def join("document:" <> document_id, params, socket) do
    {:ok, assign(socket, :document_id, document_id)}
  end
end
