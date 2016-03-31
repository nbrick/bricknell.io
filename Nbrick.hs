module Nbrick where

import qualified Data.Map.Strict as Map
import Dont

page titleText content =
  html
    [ metadata [ title titleText ]
    , body
        ([ h1 [ text titleText ] ] ++ content)
    ]

pages = Map.fromList -- Static things.
  [ ( "about", page "about me"
        [ p [ text "stuff about me" ]
        ] )
  , ( "what-i-understand", page "things (I think) I understand"
        [ h2 [ text "things I understand" ]
        , ul [ li [ text "undergraduate-level physics" ]
             , li [ text "Python" ]
             , li [ text "moderately simple C programming" ]
             , li [ text "some bits of quantum computing" ]
             ]
        ] )
  , ( "another-page", page "another page!"
        [ p [ text "this is some more text..." ]
        ] )
  ]
