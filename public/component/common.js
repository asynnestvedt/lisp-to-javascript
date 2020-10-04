/**
 * this file contains some component base classes.
 */
import { Hub as hub } from "../lib/pubsubhub.js"

/* id generation for templating */
let idcount = 0
const makeid = (prefix) => `${prefix || "gen"}${idcount++}`

export class ShadowComponent extends HTMLElement {
    constructor(id) {
        super()
        const name = this.constructor.name.toLowerCase()
        this.id = id || makeid(name)
        this.attachShadow({ mode: "open" })
        this.__template = document.createElement("template")
        this.__template.innerHTML = this.__proto__.constructor.template()
        this.shadowRoot.appendChild(this.__template.content.cloneNode(true))
        this.__initialized = false
        this.topics = []
    }

    listen() {
        hub.subscribe(this.topics, this)
    }

    connectedCallback() {
        this.listen()
    }

    disconnectedCallback() {
        hub.unsubscribe(this.topics, this)
    }

    publish(topic, payload) {
        hub.publish(topic, payload)
    }

    onMessage() {
        console.log("override this method")
    }


    hide() { 
        if( ! this.classList.contains("hidden") ) {
            this.classList.add("hidden")
        }
    }

    show() {
        if( this.classList.contains("hidden") ) {
            this.classList.remove("hidden")
        }
    }

    toggle() {
        if( this.classList.contains("hidden") ) {
            this.classList.remove("hidden")
        } else {
            this.classList.add("hidden")
        }
    }
}

export class Component extends HTMLElement {
    constructor(id) {
        super()
        const name = this.constructor.name.toLowerCase()
        this.id = id || makeid(name)
        this.__template = document.createElement("template")
        this.__template.innerHTML = this.__proto__.constructor.template()
        this.appendChild(this.__template.content.cloneNode(true))
        this.__hub = hub
        this.__initialized = false
    }

    listen() {
        hub.subscribe(this.topics, this)
    }

    connectedCallback() {
        this.listen()
    }

    disconnectedCallback() {
        hub.unsubscribe(this.topics, this)
    }

    publish(topic, payload) {
        hub.publish(topic, payload)
    }

    onMessage() {
        console.log("override this method")
    }

    hide() { 
        if( ! this.classList.contains("hidden") ) {
            this.classList.add("hidden")
        }
    }

    show() {
        if( this.classList.contains("hidden") ) {
            this.classList.remove("hidden")
        }
    }

    toggle() {
        if( this.classList.contains("hidden") ) {
            this.classList.remove("hidden")
        } else {
            this.classList.add("hidden")
        }
    }
    
}