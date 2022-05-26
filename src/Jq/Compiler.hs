module Jq.Compiler where
    

import           Jq.Filters
import           Jq.Json
import qualified Data.Map as Map
import Data.Either


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]

compile (Identity) inp = return [inp]
compile (Parenthesis filter) inp = compile filter inp


compile (Identifier identifier) (JObject entry_map) = return (identifierHelper (Map.lookup identifier entry_map))
compile (Identifier identifier) JNull  = return [JNull]
compile (Identifier identifier) _ =  Left "Error - there is no object to access"

compile (OptionalIdentifier optionalIdentifier) (JObject entry_map) = return (identifierHelper (Map.lookup optionalIdentifier entry_map))
compile (OptionalIdentifier optionalIdentifier) JNull  = return [JNull]
compile (OptionalIdentifier optionalIdentifier) _ = return []              


compile (ArrayIndex index) (JArray array) = if isInt index then arrayIndexHelper (round index) array else return [JNull]
compile (ArrayIndex index) JNull  = return [JNull]
compile (ArrayIndex index) _ =  Left "Error - there is no array to access the index"

compile (OptionalArrayIndex index) (JArray array) = if isInt index then arrayIndexHelper (round index) array else return [JNull]
compile (OptionalArrayIndex index) JNull  = return [JNull]
compile (OptionalArrayIndex index) _ = return []


compile (ArraySlice (lower_index, higher_index)) (JArray arrays) = if checked_lower < checked_upper && (lower_index /= higher_index) then return [(subArraySlice checked_lower checked_upper arrays)] else return [JArray []] where
    checked_lower = if lower_index < 0 then (length arrays) + (floor lower_index) else (floor lower_index)
    checked_upper = if higher_index < 0 then (length arrays) + (ceiling higher_index) else (if higher_index == 0 then (length arrays) else (ceiling  higher_index))
compile (ArraySlice (lower_index, higher_index)) (JString string) = if checked_lower < checked_upper && (lower_index /= higher_index) then return [(subStringSlice checked_lower checked_upper string)] else return [JArray []] where
    checked_lower = if lower_index < 0 then (length string) + (floor lower_index) else (floor lower_index)
    checked_upper = if higher_index < 0 then (length string) + (ceiling  higher_index) else (if higher_index == 0 then (length string) else (ceiling  higher_index))
compile (ArraySlice (lower_index, higher_index)) JNull = return [JNull]
compile (ArraySlice (lower_index, higher_index)) _ = Left "Error - there is no array to slice"

compile (OptionalArraySlice (lower_index, higher_index)) (JArray arrays) = if checked_lower < checked_upper && (lower_index /= higher_index) then return [(subArraySlice checked_lower checked_upper arrays)] else return [JArray []] where
    checked_lower = if lower_index < 0 then (length arrays) + (floor lower_index) else (floor lower_index)
    checked_upper = if higher_index < 0 then (length arrays) + (ceiling  higher_index) else (if higher_index == 0 then (length arrays) else (ceiling  higher_index))
compile (OptionalArraySlice (lower_index, higher_index)) (JString string) = if checked_lower < checked_upper && (lower_index /= higher_index) then return [(subStringSlice checked_lower checked_upper string)] else return [JArray []] where
    checked_lower = if lower_index < 0 then (length string) + (floor lower_index) else (floor lower_index)
    checked_upper = if higher_index < 0 then (length string) + (ceiling  higher_index) else (if higher_index == 0 then (length string) else (ceiling  higher_index))
compile (OptionalArraySlice (lower_index, higher_index)) JNull = return [JNull]
compile (OptionalArraySlice (lower_index, higher_index)) _ = return []


compile (Iterator iterations) (JArray array) = iteratorArrayHelper iterations array
compile (Iterator iterations) (JObject map) = iteratorObjectHelper iterations map
compile (Iterator iterations) _ = Left "Error - there is no array or object to iterate over"

compile (OptionalIterator iterations) (JArray array) = iteratorArrayHelper iterations array
compile (OptionalIterator iterations) (JObject map) = iteratorObjectHelper iterations map
compile (OptionalIterator iterations) _ = return []


compile (Pipe (first_operator, second_operator)) inp = pipeHelper second_operator (compile first_operator inp)
compile (Comma (first_operator, second_operator)) inp = commaHelper (compile first_operator inp) (compile second_operator inp)


compile (Value json) _ = return [json]
compile (ArrayValue filters) inp = combineJProgramsIntoArray (map (compile_reverse inp) filters)
compile (ObjectValue values) inp = objectValueHelper1 (map (\x -> compile x inp) (map (\x -> fst(x)) values)) (map (\x -> compile x inp) (map (\x -> snd(x)) values))



compile (Equality (filter_one, filter_two)) inp = equalityHelper (compile filter_one inp)  (compile filter_two inp)
compile (Inequality (filter_one, filter_two)) inp = inequalityHelper (compile filter_one inp)  (compile filter_two inp)

compile (LessThan (filter_one, filter_two)) inp = lessthanHelper (compile filter_one inp)  (compile filter_two inp)
compile (LessThanEqual (filter_one, filter_two)) inp = lessthanEqualHelper (compile filter_one inp)  (compile filter_two inp)
compile (LargerThan (filter_one, filter_two)) inp = largerthanHelper (compile filter_one inp)  (compile filter_two inp)
compile (LargerThanEqual (filter_one, filter_two)) inp = largerthanEqualHelper (compile filter_one inp)  (compile filter_two inp)

compile (IfThanElse (filter_one, filter_two, filter_three)) inp = ifThanElseHelper (compile filter_one inp) filter_two filter_three inp
compile (And (filter_one, filter_two)) inp = andHelper (compile filter_one inp) (compile filter_two inp)
compile (Or (filter_one, filter_two)) inp = orHelper (compile filter_one inp) (compile filter_two inp)
compile Not inp = notHelper (jsonToBooelan inp)


compile (Pipe2 operations) inp = pipe2Helper operations [inp]

pipe2Helper :: [Filter] -> [JSON] -> Either String [JSON]
pipe2Helper (x:xs) inp =  if null (lefts results) then pipe2Helper xs (concat (rights results)) else Left (foldl ((++) . (++ "\n")) "" (lefts results)) where 
    results = map (\json-> compile x json) inp
pipe2Helper [] inp = Right inp

objectValueHelper1 :: [Either String [JSON]] ->  [Either String [JSON]] ->  Either String [JSON]
objectValueHelper1 results_1 results_2 = if null (lefts results_1) && null (lefts results_2) then first_case else second_case where
    first_case = objectValueHelper2 (objectValueHelper3 (concat (rights results_1))) (concat (rights results_2)) --deal with concat here for different objects
    second_case = Left( foldl ((++) . (++ "\n")) "" (lefts results_1 ++ lefts results_2))

objectValueHelper2 :: [Either String String] ->  [JSON] ->  Either String [JSON]
objectValueHelper2 results_1 results_2 = if null (lefts results_1) then first_case else second_case where
    first_case = return [JObject (createMapForObject (rights  results_1) results_2)]
    second_case = Left( foldl ((++) . (++ "\n")) "" (lefts results_1))

objectValueHelper3 :: [JSON] -> [Either String String]
objectValueHelper3 xs = map objectValueHelper4 xs

objectValueHelper4 :: JSON -> Either String String
objectValueHelper4 (JString string) = Right (string)
objectValueHelper4 _ = Left ("The key of an object needs to be a string")


notHelper :: Bool -> Either String [JSON]
notHelper False = Right [JBoolean True]
notHelper True = Right [JBoolean False]

orHelper :: Either String [JSON] -> Either String [JSON] -> Either String [JSON]
orHelper (Right json_1) (Right json_2) = Right (orHelper2 (map jsonToBooelan json_1) (map jsonToBooelan json_2))
orHelper (Left error_1) (Right json_2) = Left error_1
orHelper (Right json_1) (Left error_2) = Left error_2
orHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

orHelper2 :: [Bool] -> [Bool] -> [JSON]
orHelper2 (x:xs) list_2 = if x == True then (JBoolean True) : (orHelper2 xs list_2) else orHelper3 (list_2)  ++ orHelper2 xs list_2
orHelper2 _ _ = []

orHelper3 :: [Bool] -> [JSON]
orHelper3 (x:xs) = if x == True then (JBoolean True) : orHelper3 xs else (JBoolean False) : orHelper3 xs
orHelper3 [] = []

andHelper :: Either String [JSON] -> Either String [JSON] -> Either String [JSON]
andHelper (Right json_1) (Right json_2) = Right (andHelper2 (map jsonToBooelan json_1) (map jsonToBooelan json_2))
andHelper (Left error_1) (Right json_2) = Left error_1
andHelper (Right json_1) (Left error_2) = Left error_2
andHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

andHelper2 :: [Bool] -> [Bool] -> [JSON]
andHelper2 (x:xs) list_2 = if x == False then (JBoolean False) : (andHelper2 xs list_2) else andHelper3 (list_2)  ++ andHelper2 xs list_2
andHelper2 _ _ = []

andHelper3 :: [Bool] -> [JSON]
andHelper3 (x:xs) = if x == True then (JBoolean True) : andHelper3 xs else (JBoolean False) : andHelper3 xs
andHelper3 [] = []


jsonToBooelan :: JSON -> Bool
jsonToBooelan (JNull) = False
jsonToBooelan (JBoolean False) = False 
jsonToBooelan _ = True

ifThanElseHelper :: Either String [JSON] -> Filter -> Filter -> JSON -> Either String [JSON]
ifThanElseHelper (Right json_1) filter_two filter_three inp = combineJPrograms (ifThanElseHelper2 json_1 (compile filter_two inp) (compile filter_three inp))
ifThanElseHelper (Left error_1) filter_two filter_three inp = Left error_1

ifThanElseHelper2 :: [JSON] -> Either String [JSON] -> Either String [JSON] -> [Either String [JSON]]
ifThanElseHelper2 (x:xs) result_filter_2 result_filter_3 = (ifThanElseHelper3 x result_filter_2 result_filter_3) : (ifThanElseHelper2 xs result_filter_2 result_filter_3)
ifThanElseHelper2 [] result_filter_2 result_filter_3 = []

ifThanElseHelper3 :: JSON -> Either String [JSON] -> Either String [JSON] -> Either String [JSON]
ifThanElseHelper3 (JNull) result_filter_2 result_filter_3 = result_filter_3
ifThanElseHelper3 (JBoolean False) result_filter_2 result_filter_3 = result_filter_3
ifThanElseHelper3 _ result_filter_2 result_filter_3 = result_filter_2


largerthanEqualHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
largerthanEqualHelper (Right json_1) (Right json_2) = Right (largerthanEqualHelper2 json_1 json_2)
largerthanEqualHelper (Left error_1) (Right json_2) = Left error_1
largerthanEqualHelper (Right json_1) (Left error_2) = Left error_2
largerthanEqualHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

largerthanEqualHelper2 :: [JSON] -> [JSON] -> [JSON]
largerthanEqualHelper2 [] second_list = []
largerthanEqualHelper2 (n:ns) second_list = (map (\x -> JBoolean (n >= x)) second_list) ++ (largerthanEqualHelper2 ns second_list)

largerthanHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
largerthanHelper (Right json_1) (Right json_2) = Right (largerthanHelper2 json_1 json_2)
largerthanHelper (Left error_1) (Right json_2) = Left error_1
largerthanHelper (Right json_1) (Left error_2) = Left error_2
largerthanHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

largerthanHelper2 :: [JSON] -> [JSON] -> [JSON]
largerthanHelper2 [] second_list = []
largerthanHelper2 (n:ns) second_list = (map (\x -> JBoolean (n > x)) second_list) ++ (largerthanHelper2 ns second_list)

lessthanEqualHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
lessthanEqualHelper (Right json_1) (Right json_2) = Right (lessthanEqualHelper2 json_1 json_2)
lessthanEqualHelper (Left error_1) (Right json_2) = Left error_1
lessthanEqualHelper (Right json_1) (Left error_2) = Left error_2
lessthanEqualHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

lessthanEqualHelper2 :: [JSON] -> [JSON] -> [JSON]
lessthanEqualHelper2 [] second_list = []
lessthanEqualHelper2 (n:ns) second_list = (map (\x -> JBoolean (n <= x)) second_list) ++ (lessthanEqualHelper2 ns second_list)

lessthanHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
lessthanHelper (Right json_1) (Right json_2) = Right (lessthanHelper2 json_1 json_2)
lessthanHelper (Left error_1) (Right json_2) = Left error_1
lessthanHelper (Right json_1) (Left error_2) = Left error_2
lessthanHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

lessthanHelper2 :: [JSON] -> [JSON] -> [JSON]
lessthanHelper2 [] second_list = []
lessthanHelper2 (n:ns) second_list = (map (\x -> JBoolean (n < x)) second_list) ++ (lessthanHelper2 ns second_list)

inequalityHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
inequalityHelper (Right json_1) (Right json_2) = Right (inequalityHelper2 json_1 json_2)
inequalityHelper (Left error_1) (Right json_2) = Left error_1
inequalityHelper (Right json_1) (Left error_2) = Left error_2
inequalityHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

inequalityHelper2 :: [JSON] -> [JSON] -> [JSON]
inequalityHelper2 [] second_list = []
inequalityHelper2 (n:ns) second_list = (map (\x -> JBoolean (x /= n)) second_list) ++ (equalityHelper2 ns second_list)

equalityHelper :: Either String [JSON] -> Either String [JSON]-> Either String [JSON]
equalityHelper (Right json_1) (Right json_2) = Right (equalityHelper2 json_1 json_2)
equalityHelper (Left error_1) (Right json_2) = Left error_1
equalityHelper (Right json_1) (Left error_2) = Left error_2
equalityHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

equalityHelper2 :: [JSON] -> [JSON] -> [JSON]
equalityHelper2 [] second_list = []
equalityHelper2 (n:ns) second_list = (map (\x -> JBoolean (x == n)) second_list) ++ (equalityHelper2 ns second_list)

createMapForObject :: [String] -> [JSON] -> Map.Map String JSON
createMapForObject keys values = createMapForObject2 (zip keys values) Map.empty 

createMapForObject2 :: [(String, JSON)] -> Map.Map String JSON -> Map.Map String JSON
createMapForObject2 [] object_map = object_map
createMapForObject2 (x:xs) object_map = createMapForObject2 xs (Map.insert (fst(x)) (snd(x)) object_map)

compile_reverse :: JSON -> Filter -> Either String [JSON]
compile_reverse json filter = compile filter json

combineJProgramsIntoArray :: [Either String [JSON]] -> Either String [JSON]
combineJProgramsIntoArray results = if null (lefts results) then first_case else second_case where
    first_case = Right ([JArray (concat (rights results))])
    second_case = Left( foldl ((++) . (++ "\n")) "" (lefts results))
    

commaHelper :: Either String [JSON] -> Either String [JSON] -> Either String [JSON]
commaHelper (Right json_1) (Right json_2) = Right (json_1 ++ json_2)
commaHelper (Left error_1) (Right json_2) = Left error_1
commaHelper (Right json_1) (Left error_2) = Left error_2
commaHelper (Left error_1) (Left error_2) = Left ("\n" ++ error_1 ++ "\n" ++ error_2)

pipeHelper :: Filter -> Either String [JSON] -> Either String [JSON]
pipeHelper second_operator (Left error) = (Left error)
pipeHelper second_operator (Right json_list) =  combineJPrograms (map (compile second_operator) json_list)

combineJPrograms :: [Either String [JSON]] -> Either String [JSON]
combineJPrograms results = if null (lefts results) then first_case else second_case where
    first_case = Right (concat (rights results))
    second_case = Left( foldl ((++) . (++ "\n")) "" (lefts results))

iteratorArrayHelper :: [Either String Float] -> [JSON] -> Either String [JSON]
iteratorArrayHelper [] array = Right (array)
iteratorArrayHelper iterations array = if null (lefts iterations) then first_case else second_case where
    first_case = combineJProgramsIntoJsonList (map (\x -> iteratorArrayHelper2 x array) (rights iterations))
    second_case = Left "You can only iterate over an array with numbers"

combineJProgramsIntoJsonList :: [Either String [JSON]] -> Either String [JSON]
combineJProgramsIntoJsonList results = if null (lefts results) then first_case else second_case where
    first_case = Right (concat (rights results))
    second_case = Left( foldl ((++) . (++ "\n")) "" (lefts results))

iteratorArrayHelper2 :: Float -> [JSON] -> Either String [JSON]
iteratorArrayHelper2 index jsons = compile (ArrayIndex index) (JArray jsons) 

iteratorObjectHelper :: [Either String Float] -> (Map.Map String JSON) -> Either String [JSON]
iteratorObjectHelper [] object_map = Right (Map.elems object_map)
iteratorObjectHelper iterations object_map = if null (rights iterations) then first_case else second_case where
    first_case = Right (concat (map (\x -> identifierHelper (Map.lookup x object_map)) (lefts iterations)))
    second_case = Left "You can only iterate over an object with strings as keys"


subArraySlice :: Int -> Int -> [JSON] -> JSON 
subArraySlice lower_index higher_index array = JArray (drop lower_index (take higher_index array))

subStringSlice :: Int -> Int -> String -> JSON 
subStringSlice lower_index higher_index string = JString (drop lower_index (take higher_index string))

arrayIndexHelper:: Int -> [JSON] -> Either String [JSON]
arrayIndexHelper index array = if index < 0 then case_negative else case_positive where
    case_negative = if (new_index >= length array) || (new_index < 0) then return [JNull] else return [array !! new_index] where
        new_index = length array + index 
    case_positive = if index >= length array then return [JNull] else return [array !! index]

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

identifierHelper :: Maybe JSON -> [JSON]
identifierHelper (Just json) = [json]
identifierHelper Nothing = [JNull]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
