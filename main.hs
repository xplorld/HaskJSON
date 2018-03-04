
module Main where

import System.IO
import MonadicParser
import Markdown

axb :: Parser (Integer, String, Integer)
axb = do {
    a <- natural;
    op <- string "x";
    b <- natural;
    return (a, op, b)
}

main :: IO ()
main = do {
    _ <- print $ parse axb  "1x234";
    _ <- print $ formatHTML $ Array [
        Strong $ Text "strong",
        Text "plain text",
        Italic $ Text "hello world"
    ];
    return ()
}