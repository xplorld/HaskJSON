module Markdown where

import Data.List

data Markdown = Array [Markdown] -- paragraphs
              | Text String -- just text
              | Strong Markdown -- **strong**
              | Italic Markdown -- *italic*
              | Strikethrough Markdown -- ~~strikethrough~~
              | Heading Int Markdown -- H1 - H6
              -- no nested lists for now
              | OrderedList [Markdown] -- 1. 2. 3. 
              | UnorderedList [Markdown] -- - - -
              | Link Markdown String -- [markdown](url:string)
              | Image String String -- ![string](url: string)
              | InlineCode String -- `map`
              | CodeBlock String -- ```\n codes \n ```
              -- table not supported for now
              | Blockquotes String -- consequent lines beginning with "> " are considered blockquotes

wrap :: String -> String -> String
wrap tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

wrapWithAttribute :: String -> String -> String -> String
wrapWithAttribute tag attribute content = "<" ++ tag ++ " " ++ attribute ++ ">" ++ content ++ "</" ++ tag ++ ">" 

formatHTML :: Markdown -> String
formatHTML (Array ms) = intercalate "" (map (wrap "p" . formatHTML) ms)  -- intercalate joins list of string
formatHTML (Text s) = s;
formatHTML (Strong m) = wrap "strong" $ formatHTML m
formatHTML (Italic m) = wrap "i" $ formatHTML m
formatHTML (Strikethrough m) = wrap "del" $ formatHTML m
formatHTML (Heading i m) = wrap ("h" ++ show i) $ formatHTML m
formatHTML (OrderedList ms) = wrap "ol" $ intercalate "" (map (wrap "li" . formatHTML) ms) 
formatHTML (UnorderedList ms) = wrap "li" $ intercalate "" (map (wrap "li" . formatHTML) ms) 
formatHTML (Link m url) = wrapWithAttribute "a" ("href=\"" ++ url ++ "\"") $ formatHTML m
formatHTML (Image alt url) = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"
formatHTML (InlineCode s) = wrap "code" s
formatHTML (CodeBlock s) = wrap "pre" s
formatHTML (Blockquotes s) = wrap "blockquote" s
