module Jq.JParser where

import Parsing.Parsing
import Jq.Json

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

parseJNull :: Parser JSON
parseJNull = do 
    _ <- string "null"
    return JNull 

parseJNumber :: Parser JSON
parseJNumber = 
    do JNumber <$> number

parseJBoolean :: Parser JSON 
parseJBoolean =
    do JBoolean <$> boolean

parseJString :: Parser JSON 
parseJString = 
    do JString <$> jsonString

parseJArray :: Parser JSON
parseJArray = 
    do JArray <$> jsonArray

parseJObject :: Parser JSON
parseJObject =
    do JObject <$> jsonObject 

parseJSON :: Parser JSON
parseJSON = 
            do 
                token $ parseJObject
            <|> 
            do 
                token $ parseJString
            <|> 
            do
                token $ parseJNumber
            <|> 
            do
                token $ parseJBoolean
            <|>
            do
                token $ parseJArray
            <|>
            do
                token $ parseJNull               

number :: Parser Float
number = 
    do
        enumber
    <|>
    do
        float
    <|>
    do
        intNumber 

jsonObject :: Parser (Map String JSON)
jsonObject = do 
    _ <- symbol "{"
    key <- token jsonString
    _ <- string ":"
    value <- parseJSON
    result_tuple <- many (do helperJsonObject)
    _ <- symbol "}"
    helperJsonObject_2 ((key, value) : result_tuple) Map.empty
    <|>
    do
    _ <- symbol "{"
    _ <- symbol "}"
    return Map.empty

helperJsonObject:: Parser (String, JSON)
helperJsonObject = do
        _ <- symbol ","
        key <- token jsonString
        _ <- string ":"
        value <- parseJSON
        return (key, value)

helperJsonObject_2 :: [(String, JSON)] -> Map String JSON -> Parser (Map String JSON)
helperJsonObject_2 [] entry_map = return entry_map
helperJsonObject_2 (x:xs) entry_map = if (Map.member (fst(x)) entry_map) then return empty() else helperJsonObject_2 xs (Map.insert (fst(x)) (snd(x)) entry_map)

jsonArray :: Parser [JSON]
jsonArray = do 
        _ <- symbol "["                             
        n <- many parseJSON                              
        ns <- many (do symbol "," >> parseJSON)
        _ <- symbol "]"
        return (n ++ ns)

float :: Parser Float 
float = do 
            start <- many digit 
            _ <- symbol "."
            end <- many digit
            if null start then return (read ("0." ++ end)) else return (read (start ++ "." ++ end))
            <|>
            do
            _ <- symbol "-"
            start <- many digit 
            _ <- symbol "."
            end <- many digit
            if null start then return (read ("-0." ++ end)) else return (read ("-" ++ start ++ "." ++ end))

enumber :: Parser Float  
enumber = do 
            start <- some digit 
            _ <- symbol "."
            end <- some digit
            enotation <- string "E+"
            end2 <- some digit
            return (read (start ++ "." ++ end ++ enotation ++ end2))
            <|>
            do
            start <- some digit 
            _ <- symbol "."
            end <- some digit
            enotation <- string "E-"
            end2 <- some digit
            return (read (start ++ "." ++ end ++ enotation ++ end2))
            <|>
            do
            _ <- char '-'
            start <- some digit 
            _ <- symbol "."
            end <- some digit
            enotation <- string "E+"
            end2 <- some digit
            return (read ("-" ++ start ++ "." ++ end ++ enotation ++ end2))
            <|>
            do
            _ <- char '-'
            start <- some digit 
            _ <- symbol "."
            end <- some digit
            enotation <- string "E-"
            end2 <- some digit
            return (read ("-" ++ start ++ "." ++ end ++ enotation ++ end2))

natNumber :: Parser Float
natNumber = do 
            xs <- some digit
            return (fromInteger (read xs))

intNumber :: Parser Float
intNumber = do 
            _ <- char '-'
            n <- natNumber
            return (-n)
            <|> 
            natNumber

boolean :: Parser Bool 
boolean = do 
            _ <- string "true"
            return (read "True")
         <|> do
            _ <- string "false"
            return (read "False")

jsonString :: Parser String
jsonString = do
               _ <- char '"'
               content <- many stringCheck
               _ <- char '"'
               return (escapeCharactersHandling content)

stringCheck :: Parser Char
stringCheck = (sat (/= '"'))

escapeCharactersHandling :: [Char] -> String
escapeCharactersHandling char_list = escapeCharactersHandling1 char_list (cleanAdjacentEscapeCharacters (elemIndices '\\' char_list))

cleanAdjacentEscapeCharacters :: [Int] -> [Int]
cleanAdjacentEscapeCharacters (x:xs) = if null xs then (x:xs) else (if (head xs) == x + 1 then x : (cleanAdjacentEscapeCharacters (drop 1 xs)) else x : (cleanAdjacentEscapeCharacters xs))
cleanAdjacentEscapeCharacters [] = []

escapeCharactersHandling1 :: [Char] -> [Int] -> String
escapeCharactersHandling1 char_list [] = char_list
escapeCharactersHandling1 char_list indices = foldr escapeCharactersHandling2 char_list indices

escapeCharactersHandling2 ::  Int -> [Char] -> String 
escapeCharactersHandling2 index char_list = if length char_list == (index + 1) then char_list else escapeCharactersHandling3 char_list index (char_list !! (index + 1)) 

escapeCharactersHandling3 :: [Char] -> Int -> Char -> String
escapeCharactersHandling3 char_list index 'b' = take index char_list ++ "\b" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index 'f' = take index char_list ++ "\f" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index 'n' = take index char_list ++ "\n" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index 'r' = take index char_list ++ "\r" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index 't' = take index char_list ++ "\t" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index '"' = take index char_list ++ "\"" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index '\\' = take index char_list ++ "\\" ++ drop (index + 2) char_list
escapeCharactersHandling3 char_list index _ = "Error - should not happen"
