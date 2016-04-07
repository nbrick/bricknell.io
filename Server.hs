module Server where

import Network.HTTP.Server
import Network.URL
import Network.URI
import ContentType
import DOM -- TODO: Remove.

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

handleWith route thisHostName _ url req = -- Discard the originating address.
  responseWith $ wrap req thisHostName $ route (url_path url) (url_params url)

wrap req thisHostName route =
  case uriRegName <$> (uriAuthority $ rqURI req) of
    Just thisHostName -> route
    _ -> Just $ Html
              $ Document [text $ "Go to " ++ thisHostName ++ "."] -- TODO: 301.
