module Server where

import Network.HTTP.Server
import Network.URL
import DOM (html) -- TODO: Remove this dependency.

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

handleWith route addr url req =
  return $ responseWith $ fmap wrap $ (route (url_path url)) (url_params url)

wrap content = "<!doctype html>\n" ++ (show $ html content)
