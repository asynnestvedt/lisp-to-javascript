const RESERVED_WORDS = ["if", "lambda", "defun", "let", "write", "setq", "loop"]
const OPERATORS = ["+", "-", "*", "/", "=", "<", ">"]

/**
 * remove comments from source
 * @param {string} source 
 */
const strip = source => source.replace(/^\s+?;.*\n?/m, "")

/**
 * break into array of control chars, symbols and keywords
 * @param {string} source 
 */
const tokenize = source => {
    const tokens = []
    let acc = ""
    let escape = quoted = false
    let parenCount = 0

    // TODO: break into lines and remove parseLispLines function

    for (let p of source) {
        if (p === "\"") {
            if (!escape) {
                quoted = !quoted
            }
        }

        if (!quoted) {
            if (p === "(" || p === ")") {
                parenCount += p === "(" ? 1 : -1
                if (acc.length > 0) {
                    tokens.push(acc)
                    acc = ""
                }
                tokens.push(p)
            } else if ((p === " " || p === "\n")) {
                if (acc.length > 0) {
                    tokens.push(acc)
                    acc = ""
                } else {
                    // let whitespace and newline fallthrough to noop
                }
            } else {
                acc += p
            }
        } else {
            acc += p
        }

        if (p === "\\" && !escape) {
            escape = true
        } else if (escape) {
            escape = false
        }

    }
    if (parenCount !== 0) {
        throw new Error(`missing ${Math.abs(parenCount)} ${parenCount > 0 ? "trailing" : "leading"} parentheses.`)
    }
    return tokens
}


/**
 * returns 2d array from 1d array when multiple "root list" lines are present
 * @param {array} source 
 */
const parseLispLines = (source) => {
    let c = last = 0
    const lines = []
    for (let i = 0; i < source.length; ++i) {
        if (source[i] === "(") { c++ } else if (source[i] === ")") { c-- }
        if (i > 0 && c === 0) {
            lines.push(source.slice(last, i + 1))
        }
    }
    return lines
}


/**
 * build descriptive AST from tokenized source
 * @param {*} source 
 * @param {*} list 
 */
const makeAst = (source, list = []) => {
    const token = source.shift()
    if (token === undefined) {
        return list.pop()
    } else if (token === "(") {
        list.push(makeAst(source, []))
        return makeAst(source, list)
    } else if (token === ")") {
        return list
    } else {
        return makeAst(source, list.concat(applyType(token)))
    }
}


/**
 * build ast node with token type info
 * @param {*} token 
 */
const applyType = token => {
    const isNumber = (token) => !isNaN(parseFloat(token))
    const typedNumber = (token) => {
        const f = parseFloat(token)
        const i = parseInt(token)
        return f == i ? i : f
    }
    const isReservedWord = (token) => RESERVED_WORDS.includes(token)
    const isString = (token) => token[0] === "\"" && token.slice(-1) === "\""
    const isOperator = (token) => OPERATORS.includes(token)

    if (isNumber(token)) {
        return { t: "number", v: typedNumber(token) }
    }
    else if (isString(token)) {
        return { t: "string", v: token }
    }
    else if (isReservedWord(token)) {
        return { t: token, v: token }
    }
    else if (isOperator(token)) {
        return { t: "operator", v: token }
    }
    else {
        return { t: "symbol", v: token }
    }
}




module.exports = {
    parse: (program) => {
        return parseLispLines(tokenize(strip(program))).map(r => makeAst(r))
    }
}