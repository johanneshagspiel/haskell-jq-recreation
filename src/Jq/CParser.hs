module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser
import Jq.Json

parseFilter :: Parser Filter
parseFilter = do
                token $ parsePipe2
              <|>
              do
                token $ parseTernaryFilter
             <|>
              do
                token $ parseBinaryFilter
             <|>
              do
                token $ parseUnaryFilter

parseUnaryFilter :: Parser Filter
parseUnaryFilter = do
                token $ parseOptionalIterator
              <|>
              do
                token $ parseIterator
              <|>
              do
                token $ parseOptionalArraySlice
              <|>
              do
                token $ parseArraySlice
              <|>
              do  
                token $ parseOptionalArrayIndex
              <|>
              do 
                token $ parseArrayIndex                
              <|>
              do 
                token $ parseOptionalIdentifier                
              <|>
              do
                token $ parseIdentifier
              <|>
              do
                token $ parseParenthesis
              <|>
              do
                token $ parseIdentity
              <|>
              do
                token $ parseValueConstruction
              <|>
              do
                token $ parseNot

parsePipe2Operations1 :: Parser Filter
parsePipe2Operations1 = do
                token $ parseOptionalIterator
              <|>
              do
                token $ parseIterator
              <|>
              do
                token $ parseOptionalArraySlice
              <|>
              do
                token $ parseArraySlice
              <|>
              do  
                token $ parseOptionalArrayIndex
              <|>
              do 
                token $ parseArrayIndex                
              <|>
              do 
                token $ parseOptionalIdentifier                
              <|>
              do
                token $ parseIdentifier

parsePipe2Operations2 :: Parser Filter
parsePipe2Operations2 = do
                token $ parseOptionalIterator2
              <|>
              do
                token $ parseIterator2
              <|>
              do
                token $ parseOptionalArraySlice2
              <|>
              do
                token $ parseArraySlice2
              <|>
              do  
                token $ parseOptionalArrayIndex2
              <|>
              do 
                token $ parseArrayIndex2
              <|>
              do 
                token $ parseOptionalIdentifier2
              <|>
              do
                token $ parseIdentifier2     

parseBinaryFilter :: Parser Filter
parseBinaryFilter = do
                token $ parsePipe
                <|>
                do
                token $ parseComma
                <|>
                do
                token $ parseEquality
                <|>
                do
                token $ parseInequality
                <|>
                do
                token $ parseLessThan
                <|>
                do
                token $ parseLessThanEqual
                <|>
                do
                token $ parseLargerThan
                <|>
                do
                token $ parseLargerThanEqual
                <|>
                do
                token $ parseAnd
                <|>
                do
                token $ parseOr

parseTernaryFilter :: Parser Filter
parseTernaryFilter = do
                token $ parseIfThanElse

parseValueConstruction :: Parser Filter 
parseValueConstruction = do
                Value <$> (token $ parseJNull)
                <|>
                do
                Value <$> (token $ parseJBoolean)
                <|>
                do
                Value <$> (token $ parseJNumber)
                <|>
                do
                Value <$> (token $ parseJString)
                <|>
                do
                token $ parseArrayValue
                <|>
                do
                token $ parseObjectValue

parsePipe2 :: Parser Filter 
parsePipe2 = do
  start_operation <- parsePipe2Operations1
  remainder_operations <- some parsePipe2Operations2
  return (Pipe2 (start_operation : remainder_operations))

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- symbol "("
  filter <- parseFilter
  _ <- symbol ")"
  return (Parenthesis filter)





parseIdentifier :: Parser Filter 
parseIdentifier = 
  do
    _ <- char '.'
    _ <- char '['
    identifier <- token jsonString   
    _ <- char ']'
    return (Identifier identifier)
  <|>
  do
    _ <- char '.'
    identifier <- token ident
    return (Identifier identifier)

parseIdentifier2 :: Parser Filter 
parseIdentifier2 = 
  do
    _ <- char '['
    identifier <- token jsonString   
    _ <- char ']'
    return (Identifier identifier)
  <|>
  do
    _ <- char '.'
    identifier <- token ident
    return (Identifier identifier)




parseOptionalIdentifier :: Parser Filter
parseOptionalIdentifier = 
  do
    _ <- char '.'
    optionalIdentifier <- ident
    _ <- char '?'
    return (OptionalIdentifier optionalIdentifier)
  <|>
  do
    _ <- char '.'
    _ <- char '['
    optionalIdentifier <- jsonString   
    _ <- char ']'
    _ <- char '?'
    return (OptionalIdentifier optionalIdentifier)

parseOptionalIdentifier2 :: Parser Filter
parseOptionalIdentifier2 = 
  do
    _ <- char '.'
    optionalIdentifier <- ident
    _ <- char '?'
    return (OptionalIdentifier optionalIdentifier)
  <|>
  do
    _ <- char '['
    optionalIdentifier <- jsonString   
    _ <- char ']'
    _ <- char '?'
    return (OptionalIdentifier optionalIdentifier)




parseArraySlice :: Parser Filter 
parseArraySlice = do
    _ <- char '.'
    _ <- char '['
    lower_index <- token number
    _ <- char ':'
    higher_index <- token number  
    _ <- char ']'
    return (ArraySlice (lower_index, higher_index))

parseArraySlice2 :: Parser Filter 
parseArraySlice2 = do
    _ <- char '['
    lower_index <- token number
    _ <- char ':'
    higher_index <- token number  
    _ <- char ']'
    return (ArraySlice (lower_index, higher_index))




parseOptionalArraySlice :: Parser Filter 
parseOptionalArraySlice = do
    _ <- char '.'
    _ <- char '['
    lower_index <- token number
    _ <- char ':'
    higher_index <- token number  
    _ <- char ']'
    _ <- char '?'
    return (OptionalArraySlice (lower_index, higher_index))

parseOptionalArraySlice2 :: Parser Filter 
parseOptionalArraySlice2 = do
    _ <- char '['
    lower_index <- token number
    _ <- char ':'
    higher_index <- token number  
    _ <- char ']'
    _ <- char '?'
    return (OptionalArraySlice (lower_index, higher_index))


  

parseOptionalArrayIndex :: Parser Filter
parseOptionalArrayIndex = do
    _ <- char '.'
    _ <- char '['
    index <- number
    _ <- char ']'
    _ <- char '?'
    return (OptionalArrayIndex index)

parseOptionalArrayIndex2 :: Parser Filter
parseOptionalArrayIndex2 = do
    _ <- char '['
    index <- number
    _ <- char ']'
    _ <- char '?'
    return (OptionalArrayIndex index)





parseArrayIndex :: Parser Filter
parseArrayIndex = do
    _ <- char '.'
    _ <- char '['
    index <- number 
    _ <- char ']'
    return (ArrayIndex index)

parseArrayIndex2 :: Parser Filter
parseArrayIndex2 = do
    _ <- char '['
    index <- number 
    _ <- char ']'
    return (ArrayIndex index)

helperArrayIndex :: [Int] -> Int
helperArrayIndex inp = read (foldr ((++) . show) "" inp)

helperArraySlice :: [Int] -> Int
helperArraySlice [] = 0
helperArraySlice inp = read (foldr ((++) . show) "" inp)





parseIterator :: Parser Filter 
parseIterator = do
  _ <- string ".["
  iterations <- iteratorHelper
  _ <- char ']'
  return (Iterator iterations)
  <|>
  do
  _ <- string ".[]"
  return (Iterator [])

parseIterator2 :: Parser Filter 
parseIterator2 = do
  _ <- char '['
  iterations <- iteratorHelper
  _ <- char ']'
  return (Iterator iterations)
  <|>
  do
  _ <- string "[]"
  return (Iterator [])





parseOptionalIterator :: Parser Filter 
parseOptionalIterator = do
  _ <- string ".["
  iterations <- iteratorHelper
  _ <- string "]?"
  return (OptionalIterator iterations)
  <|>
  do
  _ <- string ".[]?"
  return (OptionalIterator [])

parseOptionalIterator2 :: Parser Filter 
parseOptionalIterator2 = do
  _ <- char '['
  iterations <- iteratorHelper
  _ <- string "]?"
  return (OptionalIterator iterations)
  <|>
  do
  _ <- string "[]?"
  return (OptionalIterator [])




iteratorHelper :: Parser [(Either String Float)]
iteratorHelper = do
  first <- token iteratorHelperInt <|> iteratorHelperString
  rest <- some (token iteratorHelper2 )
  return (first : rest)

iteratorHelper2 :: Parser (Either String Float)
iteratorHelper2 = do
  _ <- char ',' 
  result <- token iteratorHelperInt <|> iteratorHelperString
  return result

iteratorHelperInt :: Parser (Either String Float)
iteratorHelperInt = do
  interator <- token number
  return (Right interator)

iteratorHelperString :: Parser (Either String Float)
iteratorHelperString = do
  interator <- token jsonString 
  return (Left interator)

-- iteratorNumber :: Parser Int 
-- iteratorNumber = do
--   int_number <- int 
--   return int_number





parsePipe :: Parser Filter
parsePipe = do 
  first_operator <- token parseUnaryFilter
  _ <- char '|'
  second_operator <- token parseFilter
  return (Pipe (first_operator, second_operator))


parseComma :: Parser Filter
parseComma = do 
  first_operator <- parseUnaryFilter
  _ <- char ','
  second_operator <- token parseFilter
  return (Comma (first_operator, second_operator))




parseEquality :: Parser Filter
parseEquality = do
  first_filter <- token parseUnaryFilter
  _ <- symbol "=="
  second_filter <- token parseUnaryFilter
  return (Equality (first_filter, second_filter))

parseInequality :: Parser Filter
parseInequality = do
  first_filter <- token parseUnaryFilter
  _ <- symbol "!="
  second_filter <- token parseUnaryFilter
  return (Inequality (first_filter, second_filter))

parseLessThan :: Parser Filter
parseLessThan = do
  first_filter <- token parseUnaryFilter
  _ <- char '<'
  second_filter <- token parseUnaryFilter
  return (LessThan (first_filter, second_filter))

parseLessThanEqual :: Parser Filter
parseLessThanEqual = do
  first_filter <- token parseUnaryFilter
  _ <- symbol "<="
  second_filter <- token parseUnaryFilter
  return (LessThanEqual (first_filter, second_filter))

parseLargerThan :: Parser Filter
parseLargerThan = do
  first_filter <- token parseUnaryFilter
  _ <- char '>'
  second_filter <- token parseUnaryFilter
  return (LargerThan (first_filter, second_filter))

parseLargerThanEqual :: Parser Filter
parseLargerThanEqual = do
  first_filter <- token parseUnaryFilter
  _ <- symbol ">="
  second_filter <- token parseUnaryFilter
  return (LargerThanEqual (first_filter, second_filter))

parseAnd :: Parser Filter
parseAnd = do
  first_filter <- token parseUnaryFilter
  _ <- symbol "and"
  second_filter <- token parseUnaryFilter
  return (And (first_filter, second_filter))

parseOr :: Parser Filter
parseOr = do
  first_filter <- token parseUnaryFilter
  _ <- symbol "or"
  second_filter <- token parseUnaryFilter
  return (Or (first_filter, second_filter))

parseNot :: Parser Filter 
parseNot = do
  _ <- symbol "not"
  return Not


parseIfThanElse :: Parser Filter
parseIfThanElse = do
  _ <- string "if"
  first_filter <- token parseFilter
  _ <- string "then"
  second_filter <- token parseFilter
  _ <- string "else"
  third_filter <- token parseFilter
  _ <- string "end"
  return (IfThanElse (first_filter, second_filter, third_filter))
  <|>
  do
  _ <- string "if"
  first_filter <- token parseFilter
  _ <- string "then"
  second_filter <- token parseFilter
  _ <- string "end"
  return (IfThanElse (first_filter, second_filter, Identity))



parseArrayValue :: Parser Filter 
parseArrayValue = do
        _ <- char '['                             
        n <- many parseFilter
        ns <- many (do symbol "," >> parseFilter)                              
        _ <- char ']'
        return (ArrayValue (n ++ ns))

parseKeyString :: Parser Filter
parseKeyString = do
  string <- token $ (jsonString <|> ident)
  return (Value (JString string))


parseObjectValue :: Parser Filter 
parseObjectValue = do
        _ <- char '{'
        first_values <- helperObjectValue
        other_values <- many (do helperObjectValue2)
        _ <- char '}'
        return (ObjectValue (first_values : other_values))
        <|>
        do
        _ <- symbol "{"
        _ <- symbol "}"
        return (ObjectValue [])


helperObjectValue :: Parser (Filter, Filter)
helperObjectValue = do
        key <- token (parseKeyString <|> parseParenthesis)
        _ <- char ':'
        value <- parseUnaryFilter
        return (key, value)
        <|>
        do
        key <- token (jsonString <|> ident)
        return (Value (JString key), Identifier key)

helperObjectValue2:: Parser (Filter, Filter)
helperObjectValue2 = do
        _ <- char ','
        tuple <- helperObjectValue
        return tuple

parseNullValue :: Parser Filter 
parseNullValue = do 
    _ <- string "null"
    return (Value JNull)


parseBooleanValue :: Parser Filter 
parseBooleanValue = do
    _ <- string "True"
    return (Value (JBoolean (read "True")))
    <|> do
    _ <- string "False"
    return (Value (JBoolean (read "False")))


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e




-- stringTest2 :: Parser Char
-- stringTest2 = sat (\x -> x /= ']' && x /= ',')