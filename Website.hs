module Website where

import qualified Data.Map.Strict as Map
import DOM

route ""     []               = Just $ Document index
route "blog" []               = Just $ Document postIndex
route "blog" [("post", slug)] = Document <$> renderPost slug
route _      _                = Nothing

nav = ul [ [ link "/" "bricknell.io" ]
         , [ link "/blog" "blog" ]
         ]

index = [ nav
        , h1 [ text "welcome" ]
        , p [ text "see ", link "blog" "a blog" ]
        ]

data Post = Post
  { postTitle   :: String
  , postDate    :: String
  , postContent :: [Node]
  }

postIndex = [ nav
            , h1 [ text "some posts for you" ]
            , ul $ map -- TODO: Sort by date.
                     (\(slug, post)
                        -> [ link ("?post=" ++ slug) (postTitle post) ])
                     (Map.assocs posts)
            ]

renderPost slug =
  fmap
    (\p -> [ nav, h1 [ text $ postTitle p ] ] ++ postContent p)
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
