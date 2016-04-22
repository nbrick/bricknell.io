module Website where

import Data.List (sortBy)
import Data.Time.Format (iso8601DateFormat)
import Data.DateTime (parseDateTime)
import Safe (headMay)
import ContentType
import DOM

route ""           []         = Just $ Html $ documentAt "." index
route "blog"       []         = Just $ Html $ documentAt "blog" postIndex
route "blog" [("post", slug)] = Html <$> documentAt "" <$> renderPost slug
route "code"       []         = Just $ Html $ documentAt "code"
                                     $ [ p [ text "Under construction" ] ]
route "elsewhere"  []         = Just $ Html $ documentAt "elsewhere"
                                     $ [ elsewhereLinks ]
route "stylesheet" []         = Just $ Stylesheet "style.css"
route _           _           = Nothing

documentAt thisUrl content = Document $ wrap thisUrl content

wrap thisUrl content =
  [ metadata [ refStylesheet "stylesheet"
             ]
  , body ([ ulWithId "site-navigation" $ map
              (\(section, title)
                 -> if section == thisUrl
                      then [ text title ]
                      else [ link section title ])
              [ (".", "bricknell.io")
              , ("blog", "/blog")
              , ("code", "/code")
              , ("elsewhere", "/elsewhere")
              ]
          ] ++ content)
  ]

index = [ p [ text "I'm Nic Bricknell. I work with computers at "
            , link "http://diamond.ac.uk" "Diamond Light Source"
            , text ", the UK's national synchrotron science facility. Before t\
                   \hat, I studied Physics at "
            , link "http://cam.ac.uk" "Cambridge"
            , text ", where I was also involved with "
            , link "http://cucats.org" "CUCaTS"
            , text ". I like programming, especially in languages with nice fe\
                   \atures like type inference. (This website "
            , link "http://github.com/nbrick/nbrick.hs/blob/master/Website.hs"
                   "is written"
            , text " in pure Haskell.)"
            ]
        ]

elsewhereLinks = ul [ [ link "http://github.com/nbrick" "GitHub" ]
                    ]

data Post = Post
  { postSlug    :: String
  , postTitle   :: String
  , postDate    :: String
  , postContent :: [Node]
  }

parsePostDate post = parseDateTime (iso8601DateFormat Nothing) $ postDate post

postIndex = [ ul $ map
                     (\post
                        -> [ link ("?post=" ++ postSlug post) (postTitle post)
                           , spanWith [("style", "float: right;")]
                               [ text $ postDate post ]
                           ])
                     posts
            ]

renderPost slug =
  fmap
    (\p -> [ spanWith [("style", "float: right;")] [ text $ postDate p ]
           , h1 [ text $ postTitle p ]
           ] ++ postContent p)
    (headMay $ filter (\p -> postSlug p == slug) posts)

posts =
  sortBy (\pA pB -> compare (parsePostDate pB) (parsePostDate pA))
    [ Post "nice-monospace-fonts"
        "My favourite monospace fonts" "2016-04-02"
        [ ul [ [ h2 [ link
                        "http://adobe-fonts.github.io/source-code-pro/"
                        "Source Code Pro" ]
               , p [ text "Size 10 all the way." ]
               ]
             , [ h2 [ link
                        "https://github.com/belluzj/fantasque-sans"
                        "Fantasque Sans Mono" ]
               , p [ text "A bit of fun, and never gets old." ]
               ]
             , [ h2 [ link "http://eastfarthing.com/luculent/" "Luculent" ]
               , p [ text "If you want *really small*, Luculent is the answer.\
                          \ See "
                   , link
                       "http://eastfarthing.com/luculent/sample4.png"
                       "the pre-hinted 5x11 px version"
                   , text "."
                   ]
               ]
             ]
        ]
    , Post "how-to-grok-monads"
        "How to grok monads" "2016-04-03"
        [ olWith [("style", "list-style: none;")]
            [ [ h2 [ text "Step 1" ]
              , p [ text "Grok "
                  , link
                      "https://en.wikibooks.org/wiki/Haskell/The_Functor_class"
                      "functors"
                  , text "."
                  ]
              ]
            , [ h2 [ text "Step 2" ]
              , p [ text "Look at the type of >>= (the \"bind\" function):<br>\
                         \>>= :: Monad m => m a -> (a -> m b) -> m b" ]
              ] -- TODO: Make the <br> hack illegal.
            , [ h2 [ text "Step 3" ]
              , p [ text "Realise that >>= is similar to fmap, but instead of \
                         \taking a function "
                  , noWrap "(a -> b)"
                  , text " like fmap does, >>= takes a function "
                  , noWrap "(a -> m b)"
                  , text "."
                  ]
              ]
            ]
        ]
    , Post "epics-wat"
        "What the hell is EPICS?!" "2015-10-22"
        [ p [ text "The following is an attempt to distill a description of "
            , link "http://www.aps.anl.gov/epics/" "EPICS"
            , text " into a few paragraphs."
            ]
        , p [ text "EPICS is a framework for controlling hardware in a network\
                   \ed environment. In practice, it has two main facets:"
            ]
        , ul [ [ p [ text "A communication protocol called Channel Access (CA)\
                          \. CA lets you read and write named variables over a\
                          \ network, without nameservers. Each variable publis\
                          \hed by a CA server at A is read/writable by anyone \
                          \at B using just the variable's name, with essential\
                          \ly no configuration, so long as UDP packets can pro\
                          \pagate between A and B. Conceptually (ignoring phys\
                          \ical topology) a CA network is flat and many-to-man\
                          \y. Anyone can be a server or client, or both. Varia\
                          \ble names should be unique within a network." ]
               ]
             , [ p [ text "Software instances of a not-quite-real-time control\
                          \ system, called EPICS Databases. \"Database\" belon\
                          \gs in scare quotes, because an EPICS Database is le\
                          \ss like a database and more like a bunch of little \
                          \predefined C functions joined up by synchronous lin\
                          \ks. An EPICS Database is typically designed to embo\
                          \dy logic about what (and when) signals should be se\
                          \nt to hardware, perhaps based on some inputs from h\
                          \ardware. Typically each EPICS Database (plus its as\
                          \sociated CA server, which serves up fields from the\
                          \ database as variables) runs in a separate process \
                          \on a Linux server. Each one is called an \"IOC\", w\
                          \hich stands for input/output controller." ]
               ]
             ]
        , p [ text "How do these fit together? Typically, a CA client writes t\
                   \o a field of an EPICS Database in a far-away IOC, causing \
                   \a chain of events within the database, resulting in hardwa\
                   \re doing things. The same CA client probably reads databas\
                   \e fields from the same IOC, to monitor the hardware activi\
                   \ty."
            ]
        , p [ text "In addition, EPICS has an ecosystem of boilerplate wrapped\
                   \s, database extensions, CA client applications (graphical \
                   \or otherwise), CA libraries, hardware drivers and more. Co\
                   \ntrol logic can be pushed up and out of the database level\
                   \ to CA client level, down into the driver level, or sidewa\
                   \ys into a database supervisor in the IOC. High-volume data\
                   \ streams can bypass CA. Proprietary vendor software can be\
                   \ wrapper with EPICS."
            ]
        , p [ text "Unfortunately, EPICS comes with a bizarre set of conventio\
                   \ns about project layout and a horrible build system with t\
                   \ens of lines of unreadable makefile boilerplate duplicated\
                   \ in almost every \"module\". Otherwise, it's pretty nice."
            ]
        ]
    ]
