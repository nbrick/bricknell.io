module Website where

import qualified Data.Map.Strict as Map
import ContentType
import DOM

route ""          []               = Just $ Html $ Document $ wrap "." index
route "blog"      []               = Just $ Html $ Document
                                          $ wrap "blog" postIndex
route "blog"      [("post", slug)] = Html <$> Document
                                          <$> wrap "-" <$> renderPost slug
route "style.css" []               = Just $ Stylesheet "style.css"
route _           _                = Nothing

wrap thisUrl content =
  [ metadata [ refStylesheet "/style.css"
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

data Post = Post
  { postTitle   :: String
  , postDate    :: String
  , postContent :: [Node]
  }

postIndex = [ ul $ map -- TODO: Sort by date.
                     (\(slug, post)
                        -> [ link ("?post=" ++ slug) (postTitle post) ])
                     (Map.assocs posts)
            ]

renderPost slug =
  fmap
    (\p -> [ h1 [ text $ postTitle p ] ] ++ postContent p)
    (Map.lookup slug posts)

posts = Map.fromList
  [ ( "my-first-blog-post",
      Post "My first blog post" "2016-04-02" -- TODO: Proper datetime.
        [ p [ text "some content" ]
        , p [ text "this is going to be some really long content, which, insid\
                   \e the source code, will be split onto multiple lines, but,\
                   \ in the browser, won't be." ]
        ]
    )
  , ( "another-post",
      Post "I sure do love to blog" "2016-04-03"
       [ p [ text "fantastic content" ]
       ]
    )
  ]
