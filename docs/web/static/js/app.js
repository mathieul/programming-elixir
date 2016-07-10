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
    const socket = new Socket("/socket")
    const editor = new Quill("#editor")
    socket.connect()

    const documentId = $(editor.container).data('document-id')
    let docChannel = socket.channel(`documents: ${documentId}`)

    docChannel.on("text_change", ({delta}) => {
      editor.updateContents(delta)
    })

    // push events
    editor.on("text-change", (delta, source) => {
      if (source !== 'user') { return }

      docChannel.push("text_change", {delta: delta})
    })

    docChannel
      .join()
      .receive("ok", response => console.log("joined!", response))
      .receive("error", error => console.log("join error", error))
  }
}

App.init()
