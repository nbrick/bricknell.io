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
  $ responseWith $ wrap
  $ case renderer of
        (Just render) -> render (url_params url)
        Nothing       -> Nothing
      where renderer = dispatch (url_path url)

dispatch "blog" = Just renderBlog
dispatch _      = Nothing

wrap (Just content) = Just $ "<!doctype html>\n" ++ (show $ html content)
wrap Nothing        = Nothing

main = server handle
