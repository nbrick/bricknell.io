import qualified Data.Map.Strict as Map
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Dont
import Nbrick

standardHeaders msg msgType =
  [ Header HdrContentLength (show $ length msg)
  , Header HdrContentEncoding "UTF-8"
  , Header HdrContentEncoding msgType
  ]

responseWith (Just content) =
  let message = show content
    in Response { rspCode = (4,0,0)
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

handle addr url req = return $ responseWith $ Map.lookup (url_path url) pages

main = server handle
