import System.Environment (getArgs)
import Network.HTTP.Server
import Server (handleWith)
import Website (route)

main = do
  args <- getArgs
  let (hostName : portString : _) = args
  serverWith defaultConfig { srvPort = fromIntegral (read portString)
                           , srvHost = hostName
                           } $ handleWith route hostName
