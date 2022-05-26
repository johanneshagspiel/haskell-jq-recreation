<img src=resources/haskell_jq_recreation_logo.png alt="Haskell JQ Clone Logo" width="296" height="91">

--------------------------------------------------------------------------------
[![BSD-3-License](https://img.shields.io/github/license/johanneshagspiel/haskell-jq-recreation)](LICENSE)
[![Top Language](https://img.shields.io/github/languages/top/johanneshagspiel/haskell-jq-recreation)](https://github.com/johanneshagspiel/haskell-jq-recreation)
[![Latest Release](https://img.shields.io/github/v/release/johanneshagspiel/haskell-jq-recreation)](https://github.com/johanneshagspiel/haskell-jq-recreation/releases/)

# Haskell JQ Recreation

This repository contains a recreation of the JSON processor jq in Haskell.

## Features

This recreation has implemented most of the functionality of the original JSON processor jq such as as:

-  handling valid JSON types including:
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
    - array index and slice `.[0]`, `.[0:10]`. 
    - array/object value iterator `.[]`, `.[1,2,3]`. 
    - optional counterparts for indexing, slicing and iterators
    - comma operator `op1 , op2`
    - pipe operator `op1 | op2` 
- simple and complex value constructors
- conditionals and comparisons including:
    - "equal" and "not equal" operators `==`, `!=`
    - if-then-else expression `if A then B else C end`.
    - comparison operators for numbers `<`, `<=`, `>`, `>=`

## Tools

| Purpose                                                        | Name                                      |
|----------------------------------------------------------------|-------------------------------------------|
| Programming language                                           | [Haskell](https://www.haskell.org/)          |

## Installation Process

It is assumed that 

## Contributors

This template for this program was originally created by:
- [Bohdan Liesnikov](https://github.com/liesnikov)

## Licence

This JQ Haskell clone was originally published under the 3-Clause BSD License, which can be found in [here](LICENSE). For this repository, the terms laid out there shall not apply to any individual that is currently enrolled at a higher education institution as a student. Those individuals shall not interact with any other part of this repository besides this README in any way by, for example cloning it or looking at its source code or have someone else interact with this repository in any way.

## References

The jq logo was taken from the [official jq website](https://stedolan.github.io/jq/jq.png) and the Haskell logo was taken from [Wikipedia](https://de.wikipedia.org/wiki/Datei:Haskell-Logo.svg). 
