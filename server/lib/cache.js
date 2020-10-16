const crypto = require("crypto")

module.exports = class Cache {
    constructor() {
        this.store = {}
        setInterval(() => {
            this.autopurge()
        }, 1000)
    }

    get(key){
        return (this.store[key] || {v: undefined})["v"]
    }

    set(key, val, expireSeconds = 60) {
        const expire = Cache.epoch() + expireSeconds
        this.store[key] = { v: val, e: expire }
    }

    autopurge() {
        for (const [key, value] of Object.entries(this.store)) {
            if(value.e <= Cache.epoch()) {
                delete this.store(key)
            }
        }
    }

    static hash(string) {
        return crypto.createHash("sha256").update(string).digest("hex")
    }

    static epoch() {
        return Math.round((new Date).getTime() / 1000)
    }
}