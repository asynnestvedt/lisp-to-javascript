const express = require("express")
const app = express()
const bodyParser = require("body-parser")
const prettier = require("prettier")
const util = require("util")
const vm = require("vm")
const path = require("path")


const parser = require("./lib/lispparser")
const jspile = require("./lib/jspile")
const Cache = require("./lib/cache")
const cache = new Cache()

app.use(bodyParser.text())
app.use("/", express.static(path.join(__dirname, "../public")))


/**
 * if req has body get hash and attach to req
 */
app.use((req, res, next)=>{
    if (typeof req.body === "string") {
        req.bodyhash = Cache.hash(req.body)
    }
    next()
})

app.post("/isValidLisp", function (req, res) {
    // TODO: move more of the AST idenitifcation from transpiler to parser.
    // TODO: ... for now rely on transpiler for syntax checking.
    const js = jspile.transpile(parser.parse(req.body.trim()))
    if (js !== "") {
        res.status(200).send()
    } else {
        res.status(400).send()
    }
})

app.post("/convertToJS", function (req, res) {
    const cached = cache.get(req.bodyhash)
    let js
    if (!cached) {
        js = jspile.transpile(parser.parse(req.body.trim()))
        if (js !== "") {
            try {
                js = prettier.format(js, {parser: "babel"})
                cache.set(req.bodyhash, js)
            }
            catch (e) { /** noop */ }
        } else {
            res.status(400).send()
        }
    } else {
        console.log("cache hit")
    }
    res.status(200).send(cached || js)
})

app.post("/execute", function (req, res) {
    // FIXME: this is a security risk. If using this code on a live server swap out JS VM for lisp interpreter
    try {
        const js = jspile.transpile(parser.parse(req.body.trim()))
        const sandbox = { globalVar: 1 }
        vm.createContext(sandbox)
        vm.runInContext(js, sandbox)
        res.status(200).send(util.inspect(sandbox))
    } catch {
        res.status(400).send()
    }
})

// The 404 Route
app.get("*", function (req, res) {
    res.status(404).send("read the docs :P")
})

app.listen(3000)
