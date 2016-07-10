defmodule Docs.DocumentChannel do
  use Docs.Web, :channel
  import SweetXml

  def join("documents:" <> document_id, _params, socket) do
    IO.puts "<<<JOIN>>>: user_id=#{inspect socket.assigns.user}"
    socket = assign(socket, :document_id, document_id)
    messages = Repo.all(from m in Message,
                        where: m.document_id == ^document_id,
                        order_by: [desc: m.inserted_at],
                        select: %{id: m.id, body: m.body},
                        limit: 100) |> Enum.reverse

    {:ok, %{messages: messages}, socket}
  end

  def handle_in("text_change", %{"delta" => delta}, socket) do
    broadcast_from! socket, "text_change", %{delta: delta}
    {:reply, :ok, socket}
  end

  def handle_in("new_message", %{"body" => body} = params, socket) do
    document = Repo.get!(Document, socket.assigns.document_id)
    changeset =
      document
      |> Ecto.build_assoc(:messages)
      |> Message.changeset(params)

    case Repo.insert(changeset) do
      {:ok, _message} ->
        broadcast! socket, "new_message", %{body: body}
        {:reply, :ok, socket}
      {:error, _changeset} ->
        {:reply, {:error, %{errors: []}}, socket}
    end
  end

  def handle_in("save", params, socket) do
    changeset =
      Document
      |> Repo.get!(socket.assigns.document_id)
      |> Document.changeset(params)

    case Repo.update(changeset) do
      {:ok, _document} ->
        {:reply, :ok, socket}
      {:error, changeset} ->
        {:reply, {:error, %{errors: changeset}}, socket}
    end
  end

  def handle_in("compute_image", params, socket) do
    input = URI.encode(params["expression"])
    url = String.to_char_list(
      "http://api.wolframalpha.com/v2/query" <>
      "?appid=#{app_id}" <>
      "&input=#{input}&format=image,plaintext"
    )
    {:ok, {_, _, body}} = :httpc.request(url)

    image_url =
      body
      |> xpath(~x"/queryresult/pod[2]/subpod/img/@src")
      |> to_string

    broadcast! socket, "insert_image", %{
      url: image_url,
      start: params["start"],
      end: params["end"]
    }

    {:reply, :ok, socket}
  end

  defp app_id,
    do: Dict.fetch!(Application.get_env(:docs, :wolfram), :app_id)
end
