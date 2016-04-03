module ContentType where

import DOM (Document)

data Content
  = Html Document
  | Stylesheet String -- Filename.
  | Textfile String -- Filename.

display (Html document) = return $ show document
display (Stylesheet filename) = do
  stylesheet <- readFile filename
  return stylesheet
display (Textfile filename) = do
  text <- readFile filename
  return text
