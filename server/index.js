const express = require("express")
const app = express()
const bodyParser = require("body-parser")
const prettier = require("prettier")
const util = require("util")
const vm = require("vm")
const path = require("path")

const parser = require("./lib/lispparser")
const jspile = require("./lib/jspile")

app.use(bodyParser.text())
// eslint-disable-next-line no-undef
app.use("/", express.static(path.join(__dirname, "../public")))

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
    let js = jspile.transpile(parser.parse(req.body.trim()))
    if (js !== "") {
        try { js = prettier.format(js, {parser: "babel"}) }
        catch (e) { /** noop */ }
        res.status(200).send(js)
    } else {
        res.status(400).send()
    }
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
