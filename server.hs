import qualified Data.Map.Strict as Map
import Network.HTTP.Server
import Network.URL
import Dont (html)
import Nbrick (renderBlog)

standardHeaders msg msgType =
  [ Header HdrContentLength (show $ length msg)
  , Header HdrContentEncoding "UTF-8"
  , Header HdrContentEncoding msgType
  ]

responseWith (Just message) =
  Response { rspCode = (4,0,0)
           , rspBody = message
           , rspHeaders = standardHeaders message "text/html"
           , rspReason = "Because you're awesome! :)"
           }
responseWith Nothing =
  let message = "404 :("
    in Response { rspCode = (4,0,4)
                , rspBody = message
                , rspHeaders = standardHeaders message "text/plain"
                , rspReason = "No content for that query!"
                }

handle addr url req = return
  $ responseWith $ fmap wrap $ (dispatch (url_path url)) (url_params url)

dispatch "blog" = renderBlog
dispatch _ = (\_ -> Nothing)

wrap content = "<!doctype html>\n" ++ (show $ html content)

main = server handle
