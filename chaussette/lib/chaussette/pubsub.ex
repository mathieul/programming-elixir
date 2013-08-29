defmodule Chaussette.Pubsub do
  defrecord Session, registrees: [], messages: [] do
    def add_registree(pid, session) do
      session.registrees([ pid | session.registrees ])
    end

    def del_registree(pid, session) do
      registrees = Enum.reject(session.registrees, &(&1 == pid))
      if length(registrees) == 0 do
        Session.new
      else
        session.registrees(registrees)
      end
    end

    def add_message(message, session) do
      remaining = Enum.take(session.messages, 9)
      session.messages([ message | remaining ])
    end
  end

  defmodule State do
    def blank, do: HashDict.new

    def add_registered(state, pid, room_id) do
      session = fetch_session(state, room_id)
      update_session(state, room_id, session.add_registree(pid))
    end

    def del_registered(state, pid, room_id) do
      session = fetch_session(state, room_id)
      update_session(state, room_id, session.del_registree(pid))
    end

    def add_message(state, message, room_id) do
      session = fetch_session(state, room_id)
      update_session(state, room_id, session.add_message(message))
    end

    def last_messages(state, room_id) do
      fetch_session(state, room_id).messages
    end

    def fetch_session(state, room_id) do
      case Dict.fetch(state, room_id) do
        { :ok, session } -> session
        :error -> Session.new
      end
    end

    def session_ids(state) do
      Dict.keys(state)
    end

    defp update_session(sessions, room_id, session), do: Dict.put(sessions, room_id, session)
  end

  use GenServer.Behaviour

  #####
  # External API

  def start_link(_) do
    :gen_server.start_link({ :local, :pubsub }, __MODULE__, nil, [])
  end

  def clear_all, do: :gen_server.cast(:pubsub, :clear_all)
  def register(room_id, pid, send_messages // false) do
    :gen_server.call(:pubsub, { :register, room_id, pid, send_messages })
  end
  def unregister(room_id, pid), do: :gen_server.call(:pubsub, { :unregister, room_id, pid })
  def session_ids, do: :gen_server.call(:pubsub, :session_ids)
  def publish(room_id, message), do: :gen_server.cast(:pubsub, { :publish, room_id, message })
  def register_list(room_id), do: :gen_server.call(:pubsub, { :register_list, room_id })
  def last_messages(room_id), do: :gen_server.call(:pubsub, { :last_messages, room_id })

  #####
  # GenServer implementation

  def init(_) do
    { :ok, State.blank }
  end

  def handle_call({ :register, room_id, pid, send_messages }, _from, state) do
    if send_messages do
      messages = Enum.reverse(State.last_messages(state, room_id))
      Enum.each messages, fn message ->
        pid <- { :pubsub_message, message }
      end
    end
    { :reply, :registered, State.add_registered(state, pid, room_id) }
  end

  def handle_call({ :unregister, room_id, pid }, _from, state) do
    { :reply, :unregistered, State.del_registered(state, pid, room_id) }
  end

  def handle_call(:session_ids, _from, state) do
    { :reply, State.session_ids(state) , state }
  end

  def handle_call({ :last_messages, room_id }, _from, state) do
    { :reply, State.last_messages(state, room_id), state }
  end

  def handle_call({ :register_list, room_id }, _from, state) do
    session = State.fetch_session(state, room_id)
    list = Enum.map(session.registrees, inspect(&1))
    { :reply, list, state }
  end

  def handle_cast(:clear_all, _state), do: { :noreply, State.blank }

  def handle_cast({ :publish, room_id, message }, state) do
    session = State.fetch_session(state, room_id)
    Enum.each session.registrees, fn pid ->
      pid <- { :pubsub_message, message }
    end
    { :noreply, State.add_message(state, message, room_id) }
  end
end
