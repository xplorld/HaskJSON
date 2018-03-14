
module Main where

import System.IO
import MonadicParser
import JSON
import JSONParser
import Data.Maybe

axb :: Parser (Integer, String, Integer)
axb = do
    a  <- natural
    op <- string "x"
    b  <- natural
    return (a, op, b)

main :: IO ()
main = do
    _ <- putStrLn $ format $ JSONObject
        [ ("hello", JSONString "world")
        , ("key"  , JSONObject [("key2", JSONNumber 12)])
        ]
    _ <- print $ parse stringCharParser "\\u1234s"
    _ <- putStrLn $ format $ fromMaybe JSONNull $ parseJSON
        "{\"hello\":\"world\",\"key\":{\"key2\":12}}"
    _ <- print $ parse
        jsonParser
        "{\"hello\":\"world\",\"key\":{\"key2\":12},\t \"a\": [1,2,3]}"
    _ <- print $ parse jsonParser "{\"a\":[1,2,3]}"
    _ <- print $ parse objectParser "{}"
    _ <- print $ parse arrayParser "[1,2,3]"
    _ <- print $ parse integerParser "12"
    _ <- print $ parse jsonParser "12 "
    _ <- print $ parse pairParser " \"hello\":\"world\""
    _ <- print $ parse spaceParser "   "
    _ <- print $ parse (separatorListParser (char ',') (char 'a')) "a,a,ba"
    _ <- print $ parse (separatorListParser (char ',') (char 'a')) "a,ba"
    _ <- print $ parse (separatorListParser (char ',') (char 'a')) "a,ba"
    _ <- print $ format $ fromMaybe JSONNull $ parseJSON
        "{\"hello\":\"world\",\"key\t\b\":{\"ke\\u0233y2\":12}}"
    return ()











