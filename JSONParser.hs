module JSONParser where

import JSON
import MonadicParser
import Control.Applicative
import Data.Maybe
import Data.Char

spaceParser :: Parser String
spaceParser = many $ satisfy isSpace

ignoreSpace :: Parser a -> Parser a
ignoreSpace p = spaceParser *> p <* spaceParser

integerParser :: Parser JSON
integerParser = fmap JSONNumber natural

-- do not support \" in string yet
quotedStringParser :: Parser String
quotedStringParser = ignoreSpace $ char '"' *> many (satisfy (/= '"')) <* char '"'

stringParser :: Parser JSON
stringParser = fmap JSONString quotedStringParser 

pairParser :: Parser (String, JSON)
pairParser = do {
    key <- quotedStringParser;
    _ <- char ':';
    val <- valueParser;
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
    xs <- separatorListParser (char ',') valueParser;
    _ <- char ']';
    return $ JSONArray xs;
}

nullParser :: Parser JSON
nullParser = fmap (const JSONNull) (string "null")

trueParser :: Parser JSON
trueParser = fmap (const $ JSONBool True) (string "true")

falseParser :: Parser JSON
falseParser = fmap (const $ JSONBool False) (string "false")


valueParser :: Parser JSON
valueParser =
    ignoreSpace value where
        value = stringParser <|>
            integerParser <|>
            objectParser <|>
            arrayParser <|>
            trueParser <|>
            falseParser <|>
            nullParser

-- http://www.ietf.org/rfc/rfc4627.txt
-- "   A JSON text is a serialized object or array."
jsonParser :: Parser JSON
jsonParser =
    ignoreSpace value where
        value = 
            objectParser <|>
            arrayParser
            

parseJSON :: String -> Maybe JSON
parseJSON s = listToMaybe $ map fst $ filter remanantIsEmpty results where
    remanantIsEmpty (_, remanant) = null remanant
    results = parse jsonParser s -- here is to change parser to the ultimate parser