Code.require_file "test_helper.exs", __DIR__

defmodule PubsubTest do
  use ExUnit.Case
  alias Chaussette.Pubsub

  teardown do
    Pubsub.clear_all && :ok
  end

  test "it returns the list of session ids with #session_ids" do
    Pubsub.register("abc123", self)
    assert Pubsub.session_ids == ["abc123"]
  end

  test "it can return the list of registered pids of a session" do
    Pubsub.register("ou812", self)
    assert Pubsub.register_list("ou812") == [inspect self]
  end

  test "it receives message published when registered" do
    Pubsub.register("ou812", self)
    Pubsub.publish("ou812", "Hello there")
    receive do
      { :pubsub_message, message } -> assert message == "Hello there"
    after 50 -> raise "Did not receive message published"
    end
  end

  test "it doesn't receive messages after it has unregistered" do
    Pubsub.register("ou812", self)
    Pubsub.unregister("ou812", self)
    Pubsub.publish("ou812", "Hello there")
    receive do
      message -> raise "Received unexpected message #{inspect message} while unregistered"
    after 50 -> assert "Nothing received"
    end
  end

  test "it receives the latest messages when registered with :send_messages true" do
    Pubsub.publish("ou812", "One")
    Pubsub.publish("ou812", "Two")
    Pubsub.register("ou812", self, true)
    receive do
      { :pubsub_message, message } -> assert message == "One"
    after 50 -> raise "Did not receive 'One'"
    end
    receive do
      { :pubsub_message, message } -> assert message == "Two"
    after 50 -> raise "Did not receive 'Two'"
    end
  end

  test "the server remembers the last 10 messages" do
    Enum.each 1..12, fn n -> Pubsub.publish("ze-room-id", "msg#{n}") end
    expected = Enum.map 12..3, fn n -> "msg#{n}" end
    assert Pubsub.last_messages("ze-room-id") == expected
    assert Pubsub.last_messages("not-set-yet") == []
  end

  test "it cleans up a session after the last registree has unregistered" do
    Pubsub.register("ou812", self)
    Pubsub.publish("ou812", "Lonely")
    Pubsub.unregister("ou812", self)
    assert Pubsub.last_messages("ou812") == []
  end
end
