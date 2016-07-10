// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"

import {Socket} from "phoenix"

const App = {
  init() {
    const socket = new Socket("/socket", {
      logger: (kind, message, data) => console.log(`${kind}: ${message}`, data)
    })
    const editor = new Quill("#editor")
    const msgContainer = $('#messages')
    const msgInput = $('#message-input')
    let saveTimer = null
    socket.connect()

    const documentId = $(editor.container).data('document-id')
    let docChannel = socket.channel(`documents:${documentId}`)

    docChannel.on("insert_image", ({url, start, end}) => {
      // editor.deleteText(start, end)
      editor.insertEmbed(end, 'image', url)
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
      const expr = editor.getText(start, end)
      docChannel.push("compute_image", {expr, start, end})
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
        console.log("joined!", response.messages)
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
