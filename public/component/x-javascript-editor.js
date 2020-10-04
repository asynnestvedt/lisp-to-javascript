import { Component } from "./common.js"
import { REQUEST_TRANSLATION_COMPLETE } from "../lib/pubsubhub.js"


export default class XJsEditor extends Component {
    constructor() {
        super()
        this.editor
        this.topics = [REQUEST_TRANSLATION_COMPLETE]
    }

    onMessage(topic, payload) {
        switch (topic) {
        case REQUEST_TRANSLATION_COMPLETE:
            this.editor.getDoc().setValue(payload)
        }
    }

    connectedCallback() {
        this.listen()
        // eslint-disable-next-line no-undef
        this.editor = CodeMirror(document.getElementById("js-editor"), {
            lineNumbers: true,
            readOnly: true,
            mode: "javascript",
            tabSize: 2,
            value: "// type in the lisp editor\n// to view javascript output here"
        })
    }

    static template() {
        return /*html*/`
            <div>
                <h3>Javascript</h3>
                <div>
                    <div id="js-editor" class="readOnly"></div>
                </div>
            </div>`.trim()
    }

}

customElements.define("x-javascript-editor", XJsEditor)


