module DOM where

data Node
  = T String
  | E String [(String, String)] [Node] -- TODO: Maybe [Node].

instance Show Node where
  show (T string) = string
  show (E tag attributes nodes) =
    let
      showAttribute (key, val) = " " ++ key ++ "='" ++ val ++ "'"
    in
         "<" ++ tag ++ mconcat (map showAttribute attributes) ++ ">"
      ++ mconcat (map show nodes)
      ++ "</" ++ tag ++ ">"

data Document = Document [Node]
instance Show Document where
  show (Document nodes) = "<!doctype html>\n" ++ (show $ E "html" [] nodes)

-- Containers
metadata = E "head" []
body = E "body" []
pWith attrs = E "p" attrs
p = pWith []
h1 = E "h1" []
h2 = E "h2" []
lWith tag attrs elements = E tag attrs $ map (E "li" []) elements
ulWith = lWith "ul"
ulWithId id elements = ulWith [("id", id)] elements
ul elements = ulWith [] elements
olWith = lWith "ol"
ol elements = olWith [] elements
spanWith = E "span"
span = spanWith []


-- Sugar
text = T
blank = T ""

-- Sugary elements
title t = E "title" [] [ text t ]
link href t = E "a" [("href", href)] [ text t ]
noWrap t = spanWith [("style", "white-space: nowrap;")] [ text t ]

-- Meta things
refStylesheet filename = E "link" [ ("href", filename)
                                  , ("rel", "stylesheet")
                                  ] []
