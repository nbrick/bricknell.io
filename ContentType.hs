module ContentType where

import DOM (Document)

data Content
  = Html Document
  | Style String -- Filename.
  | Text String -- Filename.
    deriving Show
