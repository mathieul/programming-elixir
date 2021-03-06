defmodule Docs.Message do
  use Docs.Web, :model

  schema "messages" do
    field :body, :string
    belongs_to :document, Docs.Document

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:body])
    |> validate_required([:body])
  end
end
