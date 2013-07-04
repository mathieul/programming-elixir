defprotocol MyCrypt do
  @only [BitString, List]
  def encrypt(collection, shift)
  def rot13(collection)
end

defimpl MyCrypt, for: BitString do
  def encrypt(string, shift) do
    characters = Enum.map binary_to_list(string), &1 + shift
    list_to_binary(characters)
  end
  def rot13(string), do: encrypt(string, 13)
end

defimpl MyCrypt, for: List do
  def encrypt(list, shift) do
    Enum.map list, &1 + shift
  end
  def rot13(list), do: encrypt(list, 13)
end
