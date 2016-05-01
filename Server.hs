module Server where

import Network.HTTP.Server
import Network.URL
import Network.URI
import Network.Socket.Internal (SockAddr)
import ContentType
import DOM -- TODO: Remove.

headers message contentType =
  [ Header HdrContentLength (show $ length message)
  , Header HdrContentType $ contentType ++ "; charset=utf-8"
  ]

responseWith :: Maybe Content -> IO (Response String)
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

type Router = String -> [(String, String)] -> Maybe Content

-- TODO: Give the routing fn more info/just a URL?
-- | Handle an HTTP request.
handleWith :: Router               -- ^ Routing fn, which defines our website.
           -> String               -- ^ Host name for this (local) machine.
           -> SockAddr             -- ^ Originating address of the request.
           -> URL                  -- ^ Requested address.
           -> Request String       -- ^ The HTTP request.
           -> IO (Response String) -- ^ Our response.
handleWith route thisHostName _ url req = -- Discard the originating address.
  responseWith $ enforceRequestedHostName req thisHostName
               $ route (url_path url) (url_params url)

-- | Perform a redirect if someone else's DNS is pointing at us.
enforceRequestedHostName :: Request String -- ^ The HTTP request.
                         -> String         -- ^ The hostname to enforce.
                         -> (Maybe Content -> Maybe Content) -- ^ The gate fn.
enforceRequestedHostName req thisHostName =
  case uriRegName <$> (uriAuthority $ rqURI req) of
    Just thisHostName -> id
    _ -> (\_ -> Just $ Html -- TODO: 301.
                     $ Document [text $ "Go to " ++ thisHostName ++ "."])
