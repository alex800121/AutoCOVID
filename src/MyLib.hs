{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module MyLib where

import Data.Text (Text, pack)
import Web.Api.WebDriver
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

getUserPass :: IO (Text, Text)
getUserPass = do
  username : passwd : _ <- map pack . lines <$> readFile ".\\user.txt"
  return (username, passwd)

type WDIO a = WebDriverT IO a

loginWith :: (Text, Text) -> WDIO ()
loginWith (username, passwd) =do
  navigateTo "https://eip.vghtpe.gov.tw"
  usernameField <- findElement CssSelector "#login_name"
  passwdField <- findElement CssSelector "#password"
  elementSendKeys username usernameField
  elementSendKeys passwd passwdField
  elementClick =<< findElement CssSelector "#loginBtn"
  wait 5000000
  void $ findElement CssSelector "#fixed_top"

goSIDAS :: Text -> WDIO ()
goSIDAS patID = do
  navigateTo "https://web9.vghtpe.gov.tw/nicc/servlet/sidas.Controller?event=Forward&action=First"
  -- switchToFrame . FrameContainingElement =<< findElement XPathSelector "/html/body/table/tbody/tr[2]/td/table/tbody/tr[3]/td[2]/a"
  -- elementClick =<< findElement XPathSelector "/html/body/table/tbody/tr[2]/td/table/tbody/tr[3]/td[2]/a"
  navigateTo "https://web9.vghtpe.gov.tw/nicc/servlet/sidas.Controller?event=Forward&action=Sidastab_New"
  wait 10000000

doABarrelRoll :: WebDriverT IO ()
doABarrelRoll = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "do a barrel roll"]
  performActions [press EnterKey]
  wait 5000000
  return ()