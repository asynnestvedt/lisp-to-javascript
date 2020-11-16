import crypto from "crypto"

export default class Cache {
    store: {[key: string]: any} = {}
    constructor() {
        setInterval(() => {
            this.autopurge()
        }, 1000)
    }

    get(key: string): any{
        return (this.store[key] || {v: undefined})["v"]
    }

    set(key: string, val: any, expireSeconds: number = 60): void {
        const expire = Cache.epoch() + expireSeconds
        this.store[key] = { v: val, e: expire }
    }

    autopurge(): void {
        for (const [key, value] of Object.entries(this.store)) {
            if(value.e <= Cache.epoch()) {
                delete this.store[key]
            }
        }
    }

    static hash(plain: string): string {
        return crypto.createHash("sha256").update(plain).digest("hex")
    }

    static epoch(): number {
        return Math.round((new Date).getTime() / 1000)
    }
}