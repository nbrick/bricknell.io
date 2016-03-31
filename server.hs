import qualified Data.Map.Strict as Map
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Dont
import Nbrick

handle addr url req =
  return $
    (\content
       -> Response { rspCode = (4,0,0) -- TODO
                   , rspBody = (show content)
                   , rspHeaders = [ Header HdrContentLength (show (length (show content)))
                                  , Header HdrContentEncoding "UTF-8"
                                  , Header HdrContentEncoding "text/html"
                                  ]
                   , rspReason = ""
                   }) $
    let slug = url_path url
      in case Map.lookup slug pages of Just content -> content
                                       Nothing -> html [ text "404" ]

main = server handle
