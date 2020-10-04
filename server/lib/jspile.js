/***************************************************************
 * Keyword Processors
 **************************************************************/

function processDefun(ast) {
    const parts = ast.slice(1)
    let acc = ""
    acc += `function ${transpile(parts.shift())}`
    acc += `(${transpile(parts.shift())})`
    let body = ""
    // eslint-disable-next-line no-undef
    while ((p = parts.shift()) !== undefined) {
        // eslint-disable-next-line no-undef
        body += transpile(p)
    }
    return acc + `{${body}}`
}

function processIf(ast) {
    const parts = ast.slice(1)
    let acc = ""
    acc += `if(${transpile(parts[0])}){${transpile(parts[1])}}`
    if (parts[2]) {
        acc += `else{${transpile(parts[2])}}`
    }
    return acc
}

function processLambda(ast) {
    const parts = ast.slice(1)
    let acc = ""
    acc += `(${parts[0].map(p => p.v).join(",")})=>`
    acc += `{return ${transpile(parts[1])}}`
    if (parts[2]) {
        acc += `(${transpile(parts[2])})`
    }
    return acc
}

function processLet(ast) {
    const parts = ast[1]
    const assigned = parts.map(pair => `${pair[0].v}=${pair[1].v}`)
    return `{const ${assigned.join(",")};${transpile(ast.slice(2))}}`
}

function processLoop(ast) {
    // loop - for - across - do 
    if (ast[1].v === "for" && ast[3].v === "across" && ast[5].v === "do") {
        return `for(let ${ast[2].v} of ${ast[4].v}) {${transpile(ast.slice(6))}}`
    }
    // loop - for - from - to
    if (ast[1].v === "for" && ast[3].v === "from" && ast[5].v === "to") {
        return `for(let ${ast[2].v}=${ast[4].v}; ${ast[2].v}<=${ast[6].v}; ++${ast[2].v}) {${transpile(ast.slice(7))}}`
    }
}

function processOperator(ast) {
    const operands = ast.slice(1)
    const atom = ast[0]
    let results = operands.map(o => {
        return transpile(o)
    })
    return `(${results.join(atom.v.replace("=", "==="))})`
}

function processSetq(ast) {
    return `const ${ast[1].v}=${transpile(ast[2])};`
}

function processWrite(ast) {
    const parts = ast.slice(1)
    return `console.log(${transpile(parts[0])});`
}

const processors = {
    defun: processDefun,
    if: processIf,
    lambda: processLambda,
    let: processLet,
    loop: processLoop,
    operator: processOperator,
    setq: processSetq,
    write: processWrite,
}


/******************************************************************
 * Transpile
 ******************************************************************/

function transpileList(ast) {
    let res = ""
    for (let i = 0; i < ast.length; ++i) {
        /*** check for a processor of that type */
        if (ast[i].t in processors) {
            return processors[ast[i].t](ast)
        }
        /*** check for a function call as 2 item list beginning with a symbol or lambda list */
        else if (ast.length >= 2
            && ((Array.isArray(ast[0]) && ast[0][0].v === "lambda") || ast[0].t === "symbol")) {
            return `(${transpile(ast[0])})(${ast.slice(1).map(atom => transpile(atom)).join(",")})`
        } else {
            res += transpile(ast[i])
        }
    }
    return res
}

function transpile(ast) {
    if (Array.isArray(ast)) {
        return transpileList(ast)
    }
    if (["symbol", "string", "number"].includes(ast.t)) {
        return ast.v
    }
    return ""
}

/**
 * wraps transpile in try/catch
 * @param {Array} ast 
 */
function transpileSafe(ast) {
    try {
        return transpile(ast)
    } catch (e) {
        console.error(e)
        return ""
    }
}

module.exports = Object.assign({ transpile: transpileSafe }, processors)
