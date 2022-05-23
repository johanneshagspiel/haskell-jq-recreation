module Jq.Filters where

import Jq.Json
data Filter = Identity | Parenthesis Filter | Identifier String | OptionalIdentifier String | ArrayIndex Float | OptionalArrayIndex Float | ArraySlice (Float, Float) | OptionalArraySlice (Float, Float) | Iterator [Either String Float] | OptionalIterator [Either String Float] | Pipe (Filter, Filter) | Pipe2 [Filter] | Comma (Filter, Filter) | Value JSON | ArrayValue [Filter] | ObjectValue [(Filter, Filter)] | Equality (Filter, Filter) | Inequality (Filter, Filter) | LessThan (Filter, Filter) | LessThanEqual (Filter, Filter) | LargerThan (Filter, Filter) | LargerThanEqual (Filter, Filter) | IfThanElse (Filter, Filter, Filter) | And (Filter, Filter) | Or (Filter, Filter) | Not

instance Show Filter where

  show (Identity) = "Identifier"
  show (Parenthesis filter) = "Parenthesis"

  show (Identifier identifier) = "Identifier"
  show (OptionalIdentifier optionalIdentifier) = "OptionalIdentifier"

  show (ArrayIndex index) = "ArrayIndex"
  show (OptionalArrayIndex index) = "OptionalArrayIndex"

  show (ArraySlice tuple) = "ArraySlice"
  show (OptionalArraySlice tuple) = "OptionalArraySlice"

  show (Iterator iterations) = "Iterator"
  show (OptionalIterator iterations) = "Optionaliterator"

  show (Pipe tuple) = "Pipe"

  show (Pipe2 list) = "Pipe2"

  show (Comma tuple) = "Comma"

  show (Value json) = show json

  show (ArrayValue values) = show "ArrayValue"

  show (ObjectValue values) = show "ObjectValue"



  show (Equality (filter_one, filter_two)) = show "Equality"

  show (Inequality (filter_one, filter_two)) = show "Inequality"

  show (LessThan (filter_one, filter_two)) = show "LessThan"

  show (LessThanEqual (filter_one, filter_two)) = show "LessThanEqual"

  show (LargerThan (filter_one, filter_two)) = show "LargerThan"

  show (LargerThanEqual (filter_one, filter_two)) = show "LargerThanEqual"

  show (IfThanElse (filter_one, filter_two, filter_three)) = show "IfThanElse"

  show (And (filter_one, filter_two)) = show "And"

  show (Or (filter_one, filter_two)) = show "Or"

  show Not = show "Not"

data Config = ConfigC {filters :: Filter}
