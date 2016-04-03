module Server where

import Network.HTTP.Server
import Network.URL
import ContentType

standardHeaders msg msgType =
  [ Header HdrContentLength (show $ length msg)
  , Header HdrContentEncoding "UTF-8"
  , Header HdrContentEncoding msgType
  ]

responseWith (Just (Html document)) = do
  message <- display $ Html document
  return Response { rspCode = (2,0,0)
                  , rspBody = message
                  , rspHeaders = standardHeaders message "text/html"
                  , rspReason = "Because you're awesome! :)"
                  }
responseWith (Just (Stylesheet filename)) = do
  message <- display $ Stylesheet filename
  return Response { rspCode = (2,0,0)
                  , rspBody = message
                  , rspHeaders = standardHeaders message "text/css"
                  , rspReason = "Because you're stylish! ;)"
                  }
responseWith (Just (Textfile filename)) = do
  message <- display $ Textfile filename
  return Response { rspCode = (2,0,0)
                  , rspBody = message
                  , rspHeaders = standardHeaders message "text/plain"
                  , rspReason = "Because you asked for it!"
                  }
responseWith Nothing = return $
  let message = "404 :("
    in Response { rspCode = (4,0,4)
                , rspBody = message
                , rspHeaders = standardHeaders message "text/plain"
                , rspReason = "No content for that query!"
                }

handleWith route addr url req =
  responseWith $ route (url_path url) (url_params url)
