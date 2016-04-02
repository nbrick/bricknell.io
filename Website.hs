module Website where

import qualified Data.Map.Strict as Map
import DOM

route ""     []     = Just $ Document index
route "blog" params = Document <$> renderBlog params
route _      _      = Nothing

nav = ul [ [ link "/" "bricknell.io" ]
         , [ link "/blog" "blog" ]
         ]

index = [ nav
        , h1 [ text "welcome" ]
        , p [ text "see ", link "blog" "a blog" ]
        ]

data BlogPost = BlogPost
  { bPostTitle   :: String
  , bPostDate    :: String
  , bPostContent :: [Node]
  }

renderBlog :: [(String, String)] -> Maybe [Node]
renderBlog [] = -- Index.
  Just [ nav
       , h1 [ text "some posts for you" ]
       , ul $ map -- TODO: Sort by date.
                (\(slug, post)
                   -> [ link ("?post=" ++ slug) (bPostTitle post) ])
                (Map.assocs blogPosts)
       ]
renderBlog [("post", slug)] =
  fmap
    (\p -> [ nav, h1 [ text $ bPostTitle p ] ] ++ bPostContent p)
    (Map.lookup slug blogPosts)
renderBlog _ = Nothing

blogPosts = Map.fromList
  [ ( "my-first-blog-post",
      BlogPost "My first blog post" "2016-04-02"
        [ p [ text "some content" ]
        ]
    )
  , ( "another-post",
      BlogPost "I sure do love to blog" "2016-04-03"
       [ p [ text "fantastic content" ]
       ]
    )
  ]
