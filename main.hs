import System.Environment (getArgs)
import Network.HTTP.Server
import Server (handleWith)
import Website (route)

main = do
  args <- getArgs
  let port = fromIntegral (read $ head args)
  serverWith defaultConfig { srvPort = port } $ handleWith route
