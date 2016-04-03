module Server where

import Network.HTTP.Server
import Network.URL
import ContentType

headers message contentType =
  [ Header HdrContentLength (show $ length message)
  , Header HdrContentEncoding "UTF-8"
  , Header HdrContentEncoding contentType
  ]

responseWith (Just content) = do
  message <- display content
  return Response { rspCode = (2,0,0)
                  , rspBody = message
                  , rspHeaders =
                      headers message $ case content of
                                          Html _       -> "text/html"
                                          Stylesheet _ -> "text/css"
                                          Textfile _   -> "text/plain"
                  , rspReason = "OK"
                  }
responseWith Nothing = do
  let message = "404 :("
  return Response { rspCode = (4,0,4)
                  , rspBody = message
                  , rspHeaders = headers message "text/plain"
                  , rspReason = "Not found."
                  }

handleWith route addr url req =
  responseWith $ route (url_path url) (url_params url)
