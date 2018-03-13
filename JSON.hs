module JSON where

import Data.List

data JSON = JSONObject [(String, JSON)] |
            JSONArray [JSON] |
            JSONString String |
            JSONNumber Integer | -- int for now, make floats later
            JSONBool Bool |
            JSONNull

instance Show JSON where
    show = format

format :: JSON -> String
format (JSONObject xs) = "{" ++ content ++ "}" where
    content = intercalate "," $ map (\(k,v) -> format (JSONString k) ++ ":" ++ format v) xs
format (JSONArray xs) = "[" ++ content ++ "]" where
    content = intercalate "," $ map format xs
format (JSONString s) = "\"" ++ s ++ "\""
format (JSONNumber n) = show n
format (JSONBool True) = "true"
format (JSONBool False) = "false"
format JSONNull = "null"
