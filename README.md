<img src=resources/haskell_jq_recreation_logo.png alt="Haskell JQ Clone Logo" width="296" height="91">

--------------------------------------------------------------------------------
[![BSD-3-License](https://img.shields.io/github/license/johanneshagspiel/haskell-jq-recreation)](LICENSE)
[![Top Language](https://img.shields.io/github/languages/top/johanneshagspiel/haskell-jq-recreation)](https://github.com/johanneshagspiel/haskell-jq-recreation)
[![Latest Release](https://img.shields.io/github/v/release/johanneshagspiel/haskell-jq-recreation)](https://github.com/johanneshagspiel/haskell-jq-recreation/releases/)

# Haskell JQ Recreation

This repository contains a recreation of the JSON processor [jq](https://stedolan.github.io/jq/) in Haskell.

## Features

This recreation has implemented most of the functionality of jq such as as:

-  handling all valid JSON types including:
    - `null`
    - numbers (including floating-point and E-notation)
    - strings (with support for escape characters)
    - Booleans
    - Arrays
    - JSON Objects
- applying all basic filters such as:  
    - the identity filter `.`
    - parenthesis '()'
    - object indexing, both identifier `.field` and generic `.["field"]`. 
    - optional object indexing `.field?` (and `.["field"]?`)
    - the array index and slice `.[0]`, `.[0:10]`. 
    - the array/object value iterator `.[]`, `.[1,2,3]`. 
    - the optional counterparts for indexing, slicing and iterators
    - the comma operator `op1 , op2`
    - the pipe operator `op1 | op2` 
- simple and complex value constructors
- handling conditionals and comparisons including:
    - "equal" and "not equal" operators `==`, `!=`
    - if-then-else expressions `if A then B else C end`.
    - comparison operatorss for numbers `<`, `<=`, `>`, `>=`

## Tools

| Purpose                                                        | Name                                      |
|----------------------------------------------------------------|-------------------------------------------|
| Programming language                                           | [Haskell](https://www.haskell.org/)       |
| Build tool                                           | [Stack](https://docs.haskellstack.org/en/stable/README/)       |

## Installation Process

It is assumed that an IDE such as [Visual Studio Code](https://code.visualstudio.com/) is installed and that the users operating system is Windows.

- Download and install the Haskell tool [Stack](https://docs.haskellstack.org/en/stable/README/)
- Open this repository in Visual Studio Code
- Run the command `stack init`, `stack build` and then `stack install` in the terminal

To run jq-clone, use `echo <input.json> | jq-clone -- <filter>` like in this example:

    echo '{"this" : 2, "that" : 3}' | jq-clone '.this > .that'

## Contributors

This template for this program was originally created by:
- [Bohdan Liesnikov](https://github.com/liesnikov)

## Licence

This JQ Haskell clone was originally published under the 3-Clause BSD License, which can be found in the [LICENSE](LICENSE) file. For this repository, the terms laid out there shall not apply to any individual that is currently enrolled at a higher education institution as a student. Those individuals shall not interact with any other part of this repository besides this README in any way by, for example cloning it or looking at its source code or have someone else interact with this repository in any way.

## References

The jq logo was taken from the [official jq website](https://stedolan.github.io/jq/jq.png) and the Haskell logo was taken from [Wikipedia](https://de.wikipedia.org/wiki/Datei:Haskell-Logo.svg). 
