module Nbrick where

import qualified Data.Map.Strict as Map
import Dont

data BlogPost = BlogPost
  { bPostTitle   :: String
  , bPostDate    :: String
  , bPostContent :: [Node]
  }

renderBlog :: [(String, String)] -> Maybe [Node]
renderBlog [] = -- Index.
  Just [ h1 [ text "some posts for you" ]
       , ul $ map -- TODO: Sort by date.
                (\slug -> [ link ("blog?post=" ++ slug) slug ])
                (Map.keys blogPosts)
       ]
renderBlog [("post", slug)] =
  let post = Map.lookup slug blogPosts
    in case post of
      (Just p) -> Just $ [ h1 [ text $ bPostTitle p ] ] ++ bPostContent p
      Nothing  -> Nothing
renderBlog _ = Nothing

blogPosts = Map.fromList
  [ ( "my-first-blog-post",
      BlogPost "My first blog post" "2016-04-02"
        [ p [ text "some content" ]
        ]
    )
  ]
