module JSONParser where

import JSON
import MonadicParser
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Char

spaceParser :: Parser String
spaceParser = many $ satisfy isSpace

ignoreSpace :: Parser a -> Parser a
ignoreSpace p = spaceParser *> p <* spaceParser

numberParser :: Parser JSON
numberParser = fmap (JSONNumber . read) numberStringParser where
    numberStringParser = concatM [signP, bodyDigitsP, fractionsP, expP]
    signP = optionalP $ char '-'
    bodyDigitsP = string "0" <|>  concatM [ (:[]) <$> satisfy isNonZeroDigit, many $ satisfy isDigit]
    fractionsP = optionalListP $ concatM [string ".", digitsP]
    expP = optionalListP $ concatM [
                    fmap (: []) $ satisfy $ flip elem "eE",
                    optionalP $ satisfy $ flip elem "+-",
                    digitsP]
    digitsP = some (satisfy isDigit)
    isNonZeroDigit c = isDigit c && (/= '0') c


stringCharParser :: Parser String
stringCharParser = ordinary <|> specialChar <|> hexUnicode
  where
    ordinary    = some $ satisfy $ flip notElem ['"', '\\']
    specialChar = do
        _ <- char '\\'
        c <- satisfy $ flip elem ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
        return ['\\', c]
    hexUnicode = do
        _         <- string "\\u"
        hexDigits <- replicateM 4 $ satisfy isHexDigit
        return $ "\\u" ++ hexDigits

quotedStringParser :: Parser String
quotedStringParser =
    ignoreSpace $ char '"' *> (concat <$> many stringCharParser) <* char '"'

stringParser :: Parser JSON
stringParser = fmap JSONString quotedStringParser

pairParser :: Parser (String, JSON)
pairParser = do
    key <- quotedStringParser
    _   <- char ':'
    val <- valueParser
    return (key, val)

objectParser :: Parser JSON
objectParser = do
    _  <- char '{'
    xs <- separatorListParser (char ',') pairParser
    _  <- char '}'
    return $ JSONObject xs

arrayParser :: Parser JSON
arrayParser = do
    _  <- char '['
    xs <- separatorListParser (char ',') valueParser
    _  <- char ']'
    return $ JSONArray xs

nullParser :: Parser JSON
nullParser = fmap (const JSONNull) (string "null")

trueParser :: Parser JSON
trueParser = fmap (const $ JSONBool True) (string "true")

falseParser :: Parser JSON
falseParser = fmap (const $ JSONBool False) (string "false")


valueParser :: Parser JSON
valueParser = ignoreSpace value
  where
    value =
        stringParser
            <|> numberParser
            <|> objectParser
            <|> arrayParser
            <|> trueParser
            <|> falseParser
            <|> nullParser

-- http://www.ietf.org/rfc/rfc4627.txt
-- "A JSON text is a serialized object or array."
jsonParser :: Parser JSON
jsonParser = ignoreSpace $ objectParser <|> arrayParser


parseJSON :: String -> Maybe JSON
parseJSON s = listToMaybe $ map fst $ filter remanantIsEmpty results
  where
    remanantIsEmpty (_, remanant) = null remanant
    results = parse jsonParser s




















