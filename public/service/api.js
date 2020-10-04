import { Hub as hub, REQUEST_TRANSLATION, REQUEST_TRANSLATION_COMPLETE } from "../lib/pubsubhub.js"

const apibase = ""

export const apiIsValidLisp = (lisp) => {
    fetch(`${apibase}/isValidLisp`, {
        method: "POST",
        headers: { "Content-Type": "text/plain" },
        body: lisp,
    }).then()
}

export const apiConvertToJs = (lisp) => {
    fetch(`${apibase}/convertToJs`, {
        method: "POST",
        headers: { "Content-Type": "text/plain" },
        body: lisp,
    }).then((response) => {
        let str = "// malformed lisp"
        response.text().then((str2) => hub.publish(REQUEST_TRANSLATION_COMPLETE, str2 || str))
    }).catch(() => {
        hub.publish(REQUEST_TRANSLATION_COMPLETE, "// malformed lisp")
    })
}

export const apiExecute = (lisp) => {
    fetch(`${apibase}/execute`, {
        method: "POST",
        headers: { "Content-Type": "text/plain" },
        body: lisp,
    }).then()
}

hub.subscribe([REQUEST_TRANSLATION], {
    onMessage: (topic, payload) => apiConvertToJs(payload)
})