# Lisp transpiler
## Overview

if you have too many lisps you can trade them in for javascripts here... this is a lisp to javascript compiler

The demo contains a lisp parser that is has limited compatibility with a subset of CommonLisp. in the server/libs folder you'll also find a JavaScript compiler library, both of which are exposed by an express API. The front-end consists of DIY web components tied together with a "redux like" pub/sub hub. Served @ http://localhost:3000.

![screenshot](https://raw.githubusercontent.com/onfleet/alan-backend-homework/master/public/ui-cap.png?token=AATXE7FAMGOHGY7HEDDWYW27QJNDY)

### Getting Started
To install the server:
```
git clone <url-to-this-repo>
cd <local-path-to-repo>
npm i
```
### Running
To run tests, follow these simple steps:
```
npm test
```
To run the server, follow these simple steps:

```
npm start
```
Open a web browser and navigate to [http://localhost:3000](http://localhost:3000)

# API Doc
## /

#### GET
##### Summary

returns an html document with interactive lisp editor

##### Responses

| Code | Description |
| ---- | ----------- |
| 200 | OK |

## /isValidLisp

#### POST
body must contain plain/text lisp string

##### Summary

An endpoint to determine whether a string is a valid lisp program

##### Responses

| Code | Description |
| ---- | ----------- |
| 200 | OK |
| 400 | Sorry but I cant understand your lisp |

## /convertToJS

#### POST
body must contain plain/text lisp string

##### Summary

An endpoint which given a valid lisp program as a string will return the equivalent javascript. When the string supplied is not a valid program, return an error.

##### Responses

| Code | Description |
| ---- | ----------- |
| 200 | OK |
| 400 | Sorry but I cant understand your lisp |

### /execute

#### POST
body must contain plain/text lisp string

##### Summary

An endpoint which given a valid lisp program as a string will attempt to convert it the equivalent javascript and execute it returning any console logs

##### Responses

| Code | Description |
| ---- | ----------- |
| 200 | OK |
| 400 | Sorry but I cant understand your lisp |

# Language Spec
<table class="data-table">
    <tr>
        <th>langauage notes</th>
        <th></th>
    </tr>
    <tr>
        <td>
            values (by assignment or expression)
        </td>
        <td>
            a value can be an expression or literal. When supplying a macro expression, it should resolve to a literal during runtime such as a boolean, string, number, JavaScript Array or JavaScript Object. Strings that contain spaces must use double quotes. escaping double quotes and parentheses is permitted. For strings that do not contain spaces or parentheses, single quotes are acceptable.
        </td>
    </tr>
    <tr>
        <td>
            string concatenation
        </td>
        <td>
            the plus(+) operator can be used for concatenation
        </td>
    </tr>
</table>


<table class="data-table">
    <tr>
        <th>Syntax</th>
        <th>Description</th>
    </tr>
        <tr>
        <td>
            ; comment
        </td>
        <td>
            <b>leave helpful messages in your code</b>
            <p>any string preceeded by a semicolon and terminated with a new-line will be ignored by the compiler</p>
        </td>
    </tr>
    <tr>
        <td>
            (defun <i>function-name</i> (<i>arguments…</i>)<br />
              <i>body…</i>)
        </td>
        <td>
            <b>define a function by name</b>
            <p><i>function-name</i>: Function names can contain letters, digits, underscores, and dollar signs (same rules as variables). They must not begin with a number.<br /><i>arguments</i>: The parentheses may include parameter names separated by spaces
            <br /><i>body</i>: May contain one or more instructions to be executed when the function is called</p>
        </td>
    </tr>
        <tr>
        <td>
            (if <i>true-or-false-test</i><br />
              <i>actions-when-true</i>
              <i>actions-when-false</i> ;optional)
        </td>
        <td>
            <b>execute code conditionally</b>
            <p><i>true-or-false-test</i>: Any expression that evaluates to truthy/falsey javascript values
            <br /><i>actions-when-true</i>: Must contain one (or more nested) instructions to be executed when condition evaluates to true
            <br /><i>actions-when-false</i>: Optional. Must contain one (or more nested) instructions to be executed when condition evaluates to false</p>
        </td>
    </tr>
    <tr>
        <td>
            (lambda (<i>arguments…</i>)<br />
              <i>body…</i>)
        </td>
        <td>
            <b>define anonymous function</b>
            <i>arguments</i>: The parentheses may include parameter names separated by spaces
            <br /><i>body</i>: May contain one or more instructions to be executed when the function is called</p>
        </td>
    </tr>
    <tr>
        <td>
            (let <i>varlist</i> <i>body…</i>)
        </td>
        <td>
            <b>declare one or more scoped variables by name</b>
            <i>varlist</i>: the varlist is a list containing one or more variable declarations in the form (<i>variable value</i>). <i>value</i> an expression or literal. When supplying an expression, it should resolve to a literal during runtime such as a boolean, string, number, JavaScript Array or JavaScript Object<br />
            <i>body</i>: May contain one or more instructions to be executed inside the scope of the let</p>
            <p>e.g. If the varlist is composed of two-element lists, the template for the let expression looks like this
            <code>
                  (let ((<i>variable value</i>)
                    (<i>variable value</i>)
                    …)
                <i>body…</i>)
            </code>
            </p>
        </td>
    </tr>
    <tr>
        <td>
            (loop for <i>variable</i> across <i>iterable</i> do<br />
              <i>body…</i>)
        </td>
        <td>
            <b>execute something(s) more than once</b>
            <p><i>variable</i>: For every iteration, the value of the next property is assigned to the variable<br />
            <i>iterable</i>: Any macro that resolves to a javascript object that has iterable properties or a javascript literal that has iterable properties<br />
            <i>body</i>: May contain one or more instructions to be executed each time the loop is executed</p>
        </td>
    </tr>
    <tr>
        <td>
            (loop for <i>variable</i> from <i>number1</i> to <i>number2</i><br />
              <i>body…</i>)
        </td>
        <td>
            <b>execute something(s) more than once</b>
            <p><i>variable</i>: For every iteration, the next value in the range is assigned to the variable<br />
            <i>number1</i>: the starting range value. usually 0 or 1
            <i>number2</i>: the ending range value<br />
            <i>body</i>: May contain one or more instructions to be executed each time the loop is executed</p>
        </td>
    </tr>
    <tr>
        <td>
            (<i>operator</i> <i>operands…</i>)
        </td>
        <td>
            <p><i>operator</i>: one of the following [+, -, *, /, =, <, >]<br />
            <i>operands</i>: two or more space delimited expressions. the equality (=) operator is a special case and requires 2 operands to produce working JavaScript</p>
        </td>
    </tr>
    <tr>
        <td>
            (setq <i>variable-name</i> <i>value</i>)
        </td>
        <td>
            <b>declare a variable by name</b>
            <p><i>variable-name</i>: Variable names can contain letters, digits, underscores, and dollar signs (same rules as functions). They must not begin with a number<br />
            <i>value</i>: an expression or literal. When supplying an expression, it must resolve to a literal during runtime such as a boolean, string, number, JavaScript Array or JavaScript Object</p>
        </td>
    </tr>
    <tr>
        <td>
            (write <i>value</i>)
        </td>
        <td>
            <b>writes a string to stdout</b>
            <p><i>value</i>: an expression or string literal. When supplying an expression, it must resolve to a string literal during runtime</p>
        </td>
    </tr>
</table>


# Developer Notes

 * compiler allows vars and function symbols to be lisp reserved words
 * doesn't check for keyword collision when converting to javascript (e.g naming a function "this")
 * very very limited loop functionality
 * "let" compiles to javascript brace block
 * does not check that symbols were defined prior to use
 * does not check that symbols are not numbers
 * can use javascript array and object literals (no function literals due to parens being interpreted as lisp lists)
 * plus(+) operator can be used for string concatenation
 * tests are very incomplete

While creating this I encountered a couple of minor challenges and a couple of larger ones. First I tried to use generator functions to step through the list heirarchy but couldn't fully wrap my head around it so I went with recursion. Next I spent some time trying to create a regex that would preserve whitespace in strings but ended up with a loop that tokenized in a single pass. The most challenging part of this project has been trying to comprehend the boundary of syntax between the lispyness and javascript. There are too many combinations to check manually so I think if I had more free time, I'd make a script that generates tests for every combination of macros, operators and literals (2 levels deep), as well as systematically testing for injected javascript.

a major blunder in this implementation was to put all the parsing logic in the transpiling library. If I ever rewrite this, it will be in golang and the parser will perform the syntax validation and produce a far more robust AST.
