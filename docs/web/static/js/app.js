import "phoenix_html"
import { Socket, LongPoll } from "phoenix"

const App = {
  init() {
    const socket = new Socket("/socket", {
      // transport: LongPoll,
      // logger: (kind, message, data) => console.log(`${kind}: ${message}`, data)
    })
    const editor = new Quill("#editor")
    const msgContainer = $('#messages')
    const msgInput = $('#message-input')
    let saveTimer = null
    socket.connect()

    const documentId = $(editor.container).data('document-id')
    let docChannel = socket.channel(`documents:${documentId}`)

    docChannel.on("insert_image", ({url, start, end}) => {
      editor.deleteText(start, end)
      editor.insertEmbed(start, 'image', url)
    })

    docChannel.on("text_change", ({delta}) => {
      editor.updateContents(delta)
    })

    docChannel.on("new_message", message => {
      this.appendMessage(msgContainer, message)
    })

    // push events
    $(editor.container).on("keydown", event => {
      if (event.which !== 13 || !event.metaKey) { return }

      const {start, end} = editor.getSelection()
      const expression = editor.getText(start, end)
      docChannel.push("compute_image", {expression, start, end})
    })
    editor.on("text-change", (delta, source) => {
      if (source !== 'user') { return }

      docChannel.push("text_change", {delta: delta})
      clearTimeout(saveTimer)
      saveTimer = setTimeout( () => {
        docChannel.push("save", {body: editor.getHTML()})
      }, 3000)
    })

    msgInput.on('keypress', event => {
      if (event.which !== 13) { return }

      docChannel.push("new_message", {body: msgInput.val()})
      msgInput.val('')
    })

    docChannel
      .join()
      .receive("ok", response => {
        for (const message of response.messages) {
          this.appendMessage(msgContainer, message)
        }
      })
      .receive("error", error => console.log("join error", error))
  },

  appendMessage(container, message) {
    container.append(`<br>${message.body}`)
    container.scrollTop(container.prop('scrollHeight'))
  }
}

App.init()
