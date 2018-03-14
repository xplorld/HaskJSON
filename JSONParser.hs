module JSONParser where

import JSON
import MonadicParser
import Control.Applicative
import Data.Maybe
import Data.Char

spaceParser :: Parser String
spaceParser = many $ satisfy isSpace

integerParser :: Parser JSON
integerParser = do {
    n <- natural;
    return $ JSONNumber n;
}

-- do not support \" in string yet
quotedStringParser :: Parser String
quotedStringParser = spaceParser *> char '"' *> many (satisfy (/= '"')) <* char '"' <* spaceParser

stringParser :: Parser JSON
stringParser = fmap JSONString quotedStringParser 

pairParser :: Parser (String, JSON)
pairParser = do {
    key <- quotedStringParser;
    _ <- char ':';
    val <- jsonParser;
    return (key, val);
}

objectParser :: Parser JSON
objectParser = do {
    _ <- char '{';
    xs <- separatorListParser (char ',') pairParser;
    _ <- char '}';
    return $ JSONObject xs;
}

arrayParser :: Parser JSON
arrayParser = do {
    _ <- char '[';
    xs <- separatorListParser (char ',') jsonParser;
    _ <- char ']';
    return $ JSONArray xs;
}

nullParser :: Parser JSON
nullParser = fmap (const JSONNull) (string "null")

trueParser :: Parser JSON
trueParser = fmap (const $ JSONBool True) (string "true")

falseParser :: Parser JSON
falseParser = fmap (const $ JSONBool False) (string "false")

jsonParser :: Parser JSON
jsonParser =
    spaceParser *> value <* spaceParser where
        value = stringParser <|>
            integerParser <|>
            objectParser <|>
            arrayParser <|>
            trueParser <|>
            falseParser
            

parseJSON :: String -> Maybe JSON
parseJSON s = listToMaybe $ map fst $ filter remanantIsEmpty results where
    remanantIsEmpty (_, remanant) = null remanant
    results = parse jsonParser s -- here is to change parser to the ultimate parser