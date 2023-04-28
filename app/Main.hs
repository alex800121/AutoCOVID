module Main where

import MyLib ( defaultChromeConfig, loginWith, runPat, handler )
import Web.Api.WebDriver
import Web.Api.WebDriver.Monad
import System.Process
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.List ((\\))
import Control.Monad (forM_, void)
import Control.Exception(catch)

main :: IO ()
main = do
  (_, _, _, r) <- createProcess $ proc "chromedriver.exe" ["--silent", "--log-path=chromedriver.log"]
  currentTime <- (\\ "-- ::") . take 19 . show <$> getZonedTime
  let 
    successLog = "successLog-" ++ currentTime ++ ".txt"
    failLog = "failLog-" ++ currentTime ++ ".txt"
    failPid = "failPid-" ++ currentTime ++ ".txt"
  writeFile successLog ""
  writeFile failLog ""
  writeFile failPid ""
  id : pw : _ <- map T.pack . lines <$> readFile "user.txt"
  patIDlist <- map T.pack . lines <$> readFile "input.txt"
  execWebDriverT defaultChromeConfig $ do
    runIsolated_ defaultChromeCapabilities $ do
      loginWith (id, pw)
      forM_ patIDlist $ \x -> catchAnyError (do
        runPat x
        liftIO $ appendFile successLog (T.unpack x ++ "\n"))
        (handler failLog x)
        (const (handler failLog x (UnexpectedValue "未知錯誤")))
        (const (handler failLog x (UnexpectedValue "未知錯誤")))
        (const (handler failLog x (UnexpectedValue "未知錯誤")))

  terminateProcess r
