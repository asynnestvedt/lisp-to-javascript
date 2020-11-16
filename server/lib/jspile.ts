type Atom = {t:string, v:string} | string | boolean | number
interface IAST extends Array<AST | Atom> {}
type AST = Atom[] | IAST;

/***************************************************************
 * Keyword Processors
 **************************************************************/

export function processDefun(ast:AST): string {
    const parts = ast.slice(1)
    let acc  = "",
        body = "",
        p
    acc += `function ${transpile(parts.shift())}`
    acc += `(${transpile(parts.shift())})`
    while ((p = parts.shift()) !== undefined) {
        body += transpile(p)
    }
    return acc + `{${body}}`
}

export function processIf(ast:AST): string {
    const parts = ast.slice(1)
    let acc = ""
    acc += `if(${transpile(parts[0])}){${transpile(parts[1])}}`
    if (parts[2]) {
        acc += `else{${transpile(parts[2])}}`
    }
    return acc
}

export function processLambda(ast:AST): string {
    const parts = ast.slice(1) || []
    let acc = ""
    acc += `(${(parts[0] || []).map(p => p.v).join(",")})=>`
    acc += `{return ${transpile(parts[1])}}`
    if (parts[2]) {
        acc += `(${transpile(parts[2])})`
    }
    return acc
}

export function processLet(ast:AST): string {
    const parts = ast[1]
    const assigned = parts.map(pair => `${pair[0].v}=${pair[1].v}`)
    return `{const ${assigned.join(",")};${transpile(ast.slice(2))}}`
}

export function processLoop(ast:AST): string {
    // loop - for - across - do 
    if (ast[1].v === "for" && ast[3].v === "across" && ast[5].v === "do") {
        return `for(let ${ast[2].v} of ${ast[4].v}) {${transpile(ast.slice(6))}}`
    }
    // loop - for - from - to
    if (ast[1].v === "for" && ast[3].v === "from" && ast[5].v === "to") {
        return `for(let ${ast[2].v}=${ast[4].v}; ${ast[2].v}<=${ast[6].v}; ++${ast[2].v}) {${transpile(ast.slice(7))}}`
    }
}

export function processOperator(ast:AST): string {
    const operands = ast.slice(1)
    const atom = ast[0]
    let results = operands.map(o => {
        return transpile(o)
    })
    return `(${results.join(atom.v.replace("=", "==="))})`
}

export function processSetq(ast:AST): string {
    return `const ${ast[1].v}=${transpile(ast[2])};`
}

export function processWrite(ast:AST): string {
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

export function transpileList(ast: AST): string {
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

export function transpile(ast: AST): string {
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
export function transpileSafe(ast:AST): string {
    try {
        return transpile(ast)
    } catch (e) {
        console.error(e)
        return ""
    }
}

export default Object.assign({ transpile: transpileSafe }, processors)
