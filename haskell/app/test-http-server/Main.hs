{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let htmlPath = case args of
        (p:_) -> p
        []    -> "test.html"
  putStrLn $ "Serving " ++ htmlPath ++ " on http://localhost:8081/test.html"
  run 8081 (app htmlPath)

app :: FilePath -> Application
app htmlPath req respond =
  case pathInfo req of
    ["test.html"] -> do
      html <- BL.readFile htmlPath
      respond $ responseLBS status200 [("Content-Type", "text/html")] html
    _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not found" 