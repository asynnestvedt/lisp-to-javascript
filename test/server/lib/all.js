const parser = require("../../../server/lib/lispparser")
const tpile = require("../../../server/lib/jspile")


// TODO: test individual keyword processors
// TODO: add test cases for each keyword
// TODO: add test cases for literals
// TODO: add js vm to ensure resulting javascript actually executes

const tally = {
    pass: 0,
    fail: 0
}
const total = () => tally.pass + tally.fail

const todo = (s) => console.log("\x1b[33m", s)
const fail = (s) => { console.log("\x1b[31m", s); tally.fail++ }
const pass = (s) => { console.log("\x1b[32m", s); tally.pass++ }
const write = s => console.log("\x1b[37m", s)

const test = (inp, match, name = "", optional = false) => {
    write(`test #${total() + 1} ${name}`)
    if (optional) {
        return todo("optional test " + (inp === match ? "passed" : "failed"))
    }
    if (inp === match) {
        return pass("passed")
    }
    if (inp !== match) {
        return fail("failed")
    }
}


/**
 * Operator tests
 */
write(">>>>>>>>>> operator tests")
test(
    tpile.transpile(parser.parse("(+ 1(* 2 2))")),
    "(1+(2*2))",
    "operators - (+ 1(* 2 2))"
)

test(
    tpile.transpile(parser.parse("(= 1(- 2 1))")),
    "(1===(2-1))",
    "operators - (= 1(- 2 1))"
)




/**
 * assignment tests
 */
write(">>>>>>>>>> variable declaration tests")
test(
    tpile.transpile(parser.parse("(setq num 5)")),
    "const num=5;",
    "setq - (setq num 5)"
)

test(
    tpile.transpile(parser.parse("(setq this 5)")),
    "",
    "setq - (setq this 5) - test for keyword collision detection",
    true
)




/**
 * let tests
 */
write(">>>>>>>>>> let tests")
test(
    tpile.transpile(parser.parse("(let ((a \"that\")(b 1)(c 0.5)))")),
    "{const a=\"that\",b=1,c=0.5;}",
    "let - string and num types"
)

test(
    tpile.transpile(parser.parse(`(let ((w "that"))
        (if (< 2 1)
            (write (+ 7 8 9))
            (write (* 3 ((lambda (a) (+ a 1)) 5))))
        (write w))`)),
    "{const w=\"that\";if((2<1)){console.log((7+8+9));}else{console.log((3*((a)=>{return (a+1)})(5)));}console.log(w);}",
    "let - with branch and writes in body"
)

test(
    tpile.transpile(parser.parse("(let ((a \"that\" sda) b (c 0.5)))")),
    "", // should return nothing for parse error
    "let - bad args test"
)

test(
    tpile.transpile(parser.parse("(let (b (c 0.5)))")),
    "", // should return nothing for parse error
    "let - bad args test"
)




/**
 * loop tests
 */
write(">>>>>>>>>> loop tests")
test(
    tpile.transpile(parser.parse(`
        (loop for c across life do 
            (write c)
            (write meh)
            (write (+ x 5)))`)),
    "for(let c of life) {console.log(c);console.log(meh);console.log((x+5));}",
    "loop test - for/across/do"
)
test(
    tpile.transpile(parser.parse(`
    (loop for x from 1 to 5
        (write (+ x 1)))`)),
    "for(let x=1; x<=5; ++x) {console.log((x+1));}",
    "loop test - for/from/to"
)


/**
 * aggregate tests
 */
write(">>>>>>>>>> aggregate tests")
test(
    tpile.transpile(parser.parse(`(setq some "thing")
    (defun meaning (life)
      (let ((meh "closuretest") (x 33))
        (loop for c across life do 
            (write c)
            (write meh)
            (write (+ x 5))
        ))
    )
    (meaning "kale")`)),
    "const some=\"thing\";function meaning(life){{const meh=\"closuretest\",x=33;for(let c of life) {console.log(c);console.log(meh);console.log((x+5));}}}(meaning)(\"kale\")", // should return nothing for parse error
    "large test 1 - multiline, defun, loop, closure, write"
)


// TODO: add JS VM execution based tests to check that produced JS is valid.


if (tally.fail > 0) {
    fail(`\n${tally.fail} tests failed`)
    process.exit(1)
} else {
    pass(`\n${tally.pass} of ${total()} tests passed!`)
}