

export const REQUEST_TRANSLATION = "REQUEST_TRANSLATION"
export const REQUEST_VALIDATION = "REQUEST_VALIDATION"
export const REQUEST_EXECUTION = "REQUEST_EXECUTION"
export const REQUEST_PENDING = "REQUEST_PENDING"
export const REQUEST_TRANSLATION_COMPLETE = "REQUEST_TRANSLATION_COMPLETE"

/**
 * a simple pub sub hub class intended for passing messages between components
 * topic is any string
 * callback should accept 3 or 4 args... (topic, payload, ?context)
 */
export default class SimplePubSubHub {
    constructor() {
        this.listeners = {}
    }

    subscribe(topics = [], context) {
        topics.forEach(function (t) {
            if (!this.listeners[t]) {
                this.listeners[t] = new Set()
            }
            this.listeners[t].add(context)
        }.bind(this))
    }

    unsubscribe(topics, context) {
        topics.forEach(function (t) {
            if (!this.listeners[t]) {
                return
            }
            this.listeners[t].remove(context)
        }.bind(this))
    }

    publish(topic, payload) {
        if (!this.listeners[topic]) { return }
        else if (this.listeners[topic].size === 0) { delete this.listeners[topic] }

        Array.from(this.listeners[topic]).forEach(context => {
            if (typeof context.onMessage === "function") {
                // TODO: create deep copies of payload
                let dupe
                if (Array.isArray(payload)) {
                    dupe = [].concat(...payload)
                } else if (["string", "boolean", "number"].includes(typeof payload)) {
                    dupe = payload
                } else {
                    dupe = Object.assign({}, payload)
                }
                context.onMessage.call(context, topic, dupe)
            }
        })
    }
}

export const Hub = new SimplePubSubHub()