{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module MyLib where

import Data.Text (Text, pack, isInfixOf)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Web.Api.WebDriver
import Control.Monad (void, forM, filterM, when)
import Data.Functor (($>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bits (Bits(xor))
import Control.Exception ( throwIO, Exception )
import Control.Monad.Catch ( Exception, onException )
import Data.Either (fromRight, fromLeft)
import Data.Maybe (fromMaybe, fromJust)
import AddrList

-- data SIDASExcept =
--   Unknown |
--   Repeat |
--   WrongPCR |
--   WrongAddress |
--   UploadErr |
--   WrongID
--   deriving Show

-- instance Exception SIDASExcept

data TestResult =
  NoResult |
  Result Bool Int
  deriving (Show, Eq, Ord)

defaultChromeConfig = defaultWebDriverConfig {
  _environment = defaultWebDriverEnvironment {
    _env = defaultWDEnv {
      _remotePort = 9515
    }
  }
}

getUserPass :: IO (Text, Text)
getUserPass = do
  username : passwd : _ <- map pack . lines <$> readFile ".\\user.txt"
  return (username, passwd)

type WDIO a = WebDriverT IO a

-- handler :: String -> T.Text -> SIDASExcept -> IO ()
-- handler fileName patID Unknown = putStrLn ("未知錯誤: " ++ T.unpack patID) >> appendFile fileName ("未知錯誤: " ++ T.unpack patID ++ "\n")
-- handler fileName patID Repeat = putStrLn ("重複通報: " ++ T.unpack patID) >> appendFile fileName  ("重複通報: " ++ T.unpack patID ++ "\n")
-- handler fileName patID WrongPCR = putStrLn ("擷取快篩/PCR錯誤: " ++ T.unpack patID) >> appendFile fileName  ("擷取快篩/PCR錯誤: " ++ T.unpack patID ++ "\n")
-- handler fileName patID WrongAddress = putStrLn ("擷取地址錯誤: " ++ T.unpack patID) >> appendFile fileName  ("擷取地址錯誤: " ++ T.unpack patID ++ "\n")
-- handler fileName patID UploadErr = putStrLn ("上傳錯誤: " ++ T.unpack patID) >> appendFile fileName  ("上傳錯誤: " ++ T.unpack patID ++ "\n")
-- handler fileName patID WrongID = putStrLn ("解讀身份證字號錯誤: " ++ T.unpack patID) >> appendFile fileName ("解讀身份證字號錯誤: " ++ T.unpack patID ++ "\n")

handler :: String -> Text -> WDError -> WDIO ()
handler fileName patID (UnexpectedValue x) = liftIO (putStrLn (T.unpack x ++ ": " ++ T.unpack patID)) >> liftIO (appendFile fileName (concat [T.unpack x, ": ", T.unpack patID, "\n"]))
handler fileName patID _ = liftIO (putStrLn ("未知錯誤: " ++ T.unpack patID)) >> liftIO (appendFile fileName (concat [T.unpack "未知錯誤: ", T.unpack patID, "\n"]))

loginWith :: (Text, Text) -> WDIO ()
loginWith (username, passwd) = do
  navigateTo "https://eip.vghtpe.gov.tw"
  usernameField <- findElement CssSelector "#login_name"
  passwdField <- findElement CssSelector "#password"
  elementSendKeys username usernameField
  elementSendKeys passwd passwdField
  elementClick =<< findElement CssSelector "#loginBtn"
  wait 5000000
  void $ findElement CssSelector "#fixed_top"

runPat :: Text -> WDIO ()
runPat patID = do
  navigateTo "https://web9.vghtpe.gov.tw/nicc/servlet/sidas.Controller?event=Forward&action=First"
  -- switchToFrame . FrameContainingElement =<< findElement XPathSelector "/html/body/table/tbody/tr[2]/td/table/tbody/tr[3]/td[2]/a"
  -- elementClick =<< findElement XPathSelector "/html/body/table/tbody/tr[2]/td/table/tbody/tr[3]/td[2]/a"
  navigateTo "https://web9.vghtpe.gov.tw/nicc/servlet/sidas.Controller?event=Forward&action=Sidastab_New"
  elementSendKeys patID =<< findElement CssSelector "#pathist"
  executeScript "$(\"#cdccode\").val(\"19CoV\");" []
  executeScript "$(\"#query\").submit();" []
  wait 1000000
  windows <- getWindowHandles
  Just mainWindow : _ <- filter (/= Nothing) <$> forM windows (\handle -> do
    switchToWindow handle
    title <- getTitle
    case title of
      "假日警示" -> closeWindow >> return Nothing
      "訊息頁" -> do
        message <- findElement CssSelector "body > center > h3" >>= getElementText
        if "重複通報" `isInfixOf` message
        then throwError (UnexpectedValue "重複通報")
        else throwError (UnexpectedValue "未知錯誤")
      "法定傳染病通報單" -> return (Just handle)
      _ -> throwError (UnexpectedValue "未知錯誤")
    )
  switchToWindow mainWindow
  let
    openLinks x = foldr (\x acc -> case (x, acc) of
      (NoResult, acc) -> acc
      (x, NoResult) -> x
      (Result _ d1, Result _ d2) -> if d1 > d2 then x else acc) NoResult <$> mapM openLink x
    openLink x = do
      switchToWindow x
      url <- getCurrentUrl
      if "Sub_Holiday_Warning" `T.isInfixOf` url
      then
        closeWindow >>
        return NoResult
      else case url of
        "https://web9.vghtpe.gov.tw/nicc/servlet/sidas.Controller?event=Sidastab&operation=Add" -> return NoResult
        _ -> do
          table <- findElement CssSelector "body > center > table > tbody"
          results' <- drop 3 <$> findElementsFromElement CssSelector "tr > td" table
          if length results' < 2 
          then executeScript "cancel();" [] >> return NoResult
          else (\x -> do
            result <- getElementText . last $ x
            date <- read @Int . filter (/= '/') . filter (/= '-') . T.unpack <$> (getElementText . last . init $ x)
            link <- findElementFromElement CssSelector "a" $ last (init results')
            lastpositive <- T.drop (T.length "javascript:") . fromRight "" <$> getElementAttribute "href" link
            executeScript lastpositive []
            if
              result == "Detected" || 
              "陽性" `isInfixOf` result || 
              "Positive" `isInfixOf` result
            then 
              return (Result True date)
            else
              return (Result False date)
            ) results'
          -- _ -> do
          --     link <- findElementFromElement  CssSelector "a" (last (tail results'))
          --     lastpositive <- T.drop (T.length "javascript:") . fromRight "" <$> getElementAttribute "href" link
          --     executeScript lastpositive []
          --     return True
  executeScript "linkPcr();" []
  wait 1000000
  windowsPCR <- getWindowHandles
  lastPCR <- openLinks windowsPCR
  switchToWindow mainWindow
  executeScript "linkFast();" []
  wait 1000000
  windowsFast <- getWindowHandles
  lastFast <- openLinks windowsFast
  switchToWindow mainWindow
  case (lastPCR, lastFast) of
    (NoResult, NoResult) -> throwError (UnexpectedValue "擷取資料錯誤")
    (NoResult, Result False _) -> throwError (UnexpectedValue "擷取資料錯誤")
    (Result False _, NoResult) -> throwError (UnexpectedValue "擷取資料錯誤")
    (Result False _, Result False _) -> throwError (UnexpectedValue "擷取資料錯誤")
    (Result False datePCR, Result True dateFast) -> when (datePCR >= dateFast) $ throwError (UnexpectedValue "擷取資料錯誤")
    (Result True datePCR, Result False dateFast) -> when (dateFast > datePCR) $ throwError (UnexpectedValue "擷取資料錯誤")
    (_, _) -> return ()
  getAddr >>= putCityCode
  correctID
  fixDataSubmit
  liftIO (putStrLn $ T.unpack patID <> " succeeded")


getAddr :: WDIO T.Text
getAddr = fromRight "" <$> (getElementAttribute "value" =<< findElement CssSelector "input[name='pataddr']")

normalizeAddr :: T.Text -> WDIO T.Text
normalizeAddr addr = do
  newWindow WindowContext >>= switchToWindow . fst
  navigateTo "https://www.map.com.tw"
  executeScript (T.concat ["$(\"#searchWord\").val(\"", addr, "\")"]) []
  executeScript "search()" []
  findElement CssSelector "#Mapframe" >>=
    findElementFromElement CssSelector "#MapLayer" >>=
    findElementFromElement CssSelector "#overlay" >>= (wait 2000000 $>) >>=
    findElementFromElement CssSelector "#customMarkinfowindow" >>= (wait 2000000 $>) >>=
    findElementFromElement CssSelector "iframe" >>=
    getElementAttribute "src" >>=
    navigateTo . T.append "https://www.map.com.tw" . fromRight "" >> wait 2000000 >>
    findElement CssSelector "#defaultInfo > table > tbody > tr:nth-child(1) > td > table > tbody > tr:nth-child(4) > td" >>=
    getElementText >>= \x -> liftIO (putStrLn (T.unpack x)) >> closeWindow >> return x

putCityCode :: T.Text -> WDIO ()
putCityCode addr = if T.length addr < 2
  then do
    executeScript "$(\"input[name='citycode']\").val(\"63000120\")" []
    void $ executeScript "$(\"input[name='pataddr']\").val(\"未知\")" []
  else do
    let
      citycode1 = foldr (\x acc -> if last x `T.isInfixOf` addr then Just (head x) else acc) Nothing addrref
    citycode <- case citycode1 of
      Just c  -> return c
      Nothing -> do
        normalAddr <- normalizeAddr addr
        let
          citycode2 = foldr (\x acc -> if last x `T.isInfixOf` normalAddr then Just (head x) else acc) Nothing addrref
        case citycode2 of
          Just d  -> return d
          Nothing -> throwError (UnexpectedValue "地址錯誤")
    void $ executeScript (T.concat ["$(\"input[name='citycode']\").val('", citycode, "')"]) []

correctID :: WDIO ()
correctID = do
  patidno <- fromRight "" <$> (findElement CssSelector "input[name='patidno']" >>=  getElementAttribute "value")
  passport <- fromRight "" <$> (findElement CssSelector "input[name='passport']" >>= getElementAttribute "value")
  catchAnyError (do
    case patidno of
      "" -> case passport of
        "" -> void $ executeScript "$(\"input[name='patidno']\").val('A000000000')" []
        _  -> return ()
      _  -> if T.unpack patidno !! 1 == '1' || T.unpack patidno !! 1 == '2'
        then return ()
        else do
          executeScript "$(\"input[name='patidno']\").val('')" []
          executeScript (T.concat ["$(\"input[name='passport']\").val(\"", patidno, "\")"]) []
          executeScript "$(\"input[name='patnatio'][value='0']\").click()" []
          executeScript "change_show(2)" []
          executeScript "$(\"input[name='patnatio'][value='1']\").click()" []
          executeScript "change_show(2)" []
          executeScript "$(\"input[name='patident'][value='0']\").click()" []
          executeScript "radioButton(reg.patident,9)" []
          executeScript "$(\"input[name='patlegal'][value='1']\").click()" []
          executeScript "radioButton(reg.patlegal,0)" []
          executeScript "$(\"select[name='country1']\").val('OTH')" []
          executeScript "change_show(3)" []
          void $ executeScript "$(\"input[name='country2']\").val('未知')" [])
    (const (throwError (UnexpectedValue "身份證字號錯誤")))
    (const (throwError (UnexpectedValue "身份證字號錯誤")))
    (const (throwError (UnexpectedValue "身份證字號錯誤")))
    (const (throwError (UnexpectedValue "身份證字號錯誤")))

fixDataSubmit :: WDIO ()
fixDataSubmit = catchAnyError (do 
  patstatu <- getElementAttribute "value" =<< findElement CssSelector "select[name='patstatu']"
  case patstatu of
    Right "2"  -> return ()
    Right "3"  -> return ()
    _         -> void $ executeScript "$(\"select[name='patstatu']\").val('1')" []
  cellphone <- fromRight "" <$> (getElementAttribute "value" =<< findElement CssSelector "input[name='cellphone']")
  pattelph <- fromRight "" <$> (getElementAttribute "value" =<< findElement CssSelector "input[name='pattelph']")
  if T.length cellphone == 10 && (take 2 (T.unpack cellphone) == "09")
  then return ()
  else do
    case T.unpack pattelph of
      ('0' : '9' : _) -> void $ executeScript (T.concat ["$(\"input[name=\'cellphone\']\").val(\"", pattelph, "\")"]) []
      _               ->
        void $ executeScript "$(\"input[name=\'cellphone\']\").val(\"0900000000\")" []
  patname <- T.take 7 . fromRight "" <$> ( getElementAttribute "value" =<< findElement CssSelector "input[name='patname']")
  executeScript (T.concat ["$(\"input[name='patname']\").val('", patname,"')"]) []
  executeScript "$(\"input[name='d19cov22'][value='0']\").click()" []
  executeScript "$(\"form[name='reg']\").submit()" []
  wait 2000000
  alerttext <- fromMaybe "" <$> getAlertText
  if "資料存檔成功" `T.isInfixOf` alerttext then dismissAlert else dismissAlert >> throwError (UnexpectedValue "上傳錯誤"))
  (const (throwError (UnexpectedValue "上傳錯誤")))
  (const (throwError (UnexpectedValue "上傳錯誤")))
  (const (throwError (UnexpectedValue "上傳錯誤")))
  (const (throwError (UnexpectedValue "上傳錯誤")))