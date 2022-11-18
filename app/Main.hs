module Main where

import MyLib
import Web.Api.WebDriver
import Web.Api.WebDriver.Monad
import System.Process
import Control.Monad.IO.Class

defaultChromeConfig = defaultWebDriverConfig {
  _environment = defaultWebDriverEnvironment {
    _env = defaultWDEnv {
      _remotePort = 9515
    }
  }
}

main :: IO ()
main = do
  -- r <- createProcess $ proc "java" ["-jar", "selenium-server-4.5.3.jar", "standalone"]
  (_, _, _, r) <- createProcess $ proc "chromedriver.exe" ["--silent"]
  execWebDriverT defaultChromeConfig $ do
    runIsolated_ defaultChromeCapabilities $ do
      loginWith =<< liftIO getUserPass
      goSIDAS ""
  terminateProcess r
  return ()
