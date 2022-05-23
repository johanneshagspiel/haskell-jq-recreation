module Jq.Json where

import Data.Map (Map)
import qualified Data.Map as Map

data JSON =
    JNull | JNumber Float | JBoolean Bool | JString String | JArray [JSON] | JObject (Map String JSON) | JOptional | JIterator [JSON]

instance Show JSON where
  show JNull = "null"  
  show (JNumber number) = stringImprovement (show number)
  show (JBoolean boolean) = booleantImprovement (show boolean)
  show (JString string) = show string
  
  show (JArray []) = "[]"
  show (JArray (x:xs)) = "[" ++ (show x) ++ (helperArray xs) ++ "]"

  show (JObject entries) = "{" ++ (helperObject (Map.toList entries)) ++ "}"

  show JOptional = ""

  show (JIterator array) = concat . map ((++"\n") . show) $ array

booleantImprovement :: String -> String
booleantImprovement "True" = "true"
booleantImprovement "False" = "false"

stringImprovement :: [Char]  -> [Char]
stringImprovement char_list = if 'e' `elem` second_part then first_part ++ "." ++ second_part else (if (read second_part) == 0 then first_part else first_part ++ "." ++ second_part) where
  first_part = takeWhile (/= '.') char_list
  second_part = drop 1 (dropWhile (/= '.') char_list)

helperArray :: [JSON] -> String
helperArray [] = ""
helperArray (x:xs) = "," ++ (show x) ++ (helperArray xs)

helperObject :: [(String, JSON)] -> String
helperObject [] = ""
helperObject (x:xs) = (show(fst x)) ++ ":" ++ (show(snd x)) ++ (helperObject_2 xs)

helperObject_2 :: [(String, JSON)] -> String
helperObject_2 [] = ""
helperObject_2 (x:xs) = "," ++ (show(fst x)) ++ ":" ++ (show(snd x)) ++ (helperObject_2 xs)

instance Eq JSON where
  (==) JNull JNull = True
  (==) (JNumber number_1) (JNumber number_2) = number_1 == number_2
  (==) (JBoolean boolean_1) (JBoolean boolean_2) = boolean_1 == boolean_2
  (==) (JString string_1) (JString string_2) = string_1 == string_2
  (==) (JArray array_1) (JArray array_2) = array_1 == array_2
  (==) (JObject map_1) (JObject map_2) = map_1 == map_2
  (==) _ _ = False 

instance Ord  JSON where
  compare  (JNumber number_1) (JNumber number_2) = compare number_1 number_2
  compare  (JBoolean boolean_1) (JBoolean boolean_2) = compare boolean_1 boolean_2
  compare  (JString string_1) (JString string_2) = compare string_1 string_2
  compare  (JArray array_1) (JArray array_2) = compare array_1 array_2
  compare (JObject map_1) (JObject map_2) = compare map_1 map_2
  compare _ _ = GT