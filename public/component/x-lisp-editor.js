

import { Component } from "./common.js"
import { Hub as hub, REQUEST_TRANSLATION } from "../lib/pubsubhub.js"

const sample = `(let ((veg "kale")(fruit "kiwi"))
(loop for x from 1 to 5
    (write (+ x 1)))
(write fruit)
(setq test 
   (lambda (arg) 
      (write (+ arg fruit))))
(test "mmm"))`

export default class XLispEditor extends Component {
    constructor() {
        super()
        this.editor
        this.timeout
    }

    connectedCallback() {
        this.listen()
        // eslint-disable-next-line no-undef
        this.editor = CodeMirror(document.getElementById("lisp-editor"), {
            lineNumbers: true,
            mode: "commonlisp",
            tabSize: 2,
            value: sample,
        })

        this.editor.on("change", this.onchange.bind(this))
    }

    onchange() {
        clearTimeout(this.timeout)

        this.timeout = setTimeout(() => {
            hub.publish(REQUEST_TRANSLATION, this.editor.getValue())
        }, 1000)
    }



    static template() {
        return /*html*/`
            <div>
                <h3>Lisp</h3>
                <div>
                    <div id="lisp-editor"></div>
                </div>
            </div>`.trim()
    }
}

customElements.define("x-lisp-editor", XLispEditor)