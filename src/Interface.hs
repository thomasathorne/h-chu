
module Interface where

import System.IO (Handle, hGetLine, hPutStrLn)
import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import qualified Data.Text as T


data Interface = Regular Handle
               | Web WS.Connection
               | AI (TChan String) (TChan String)


getString :: Interface -> IO String
getString (Regular h) = hGetLine h
getString (Web conn) = do
 inst <- (WS.receiveData conn :: IO T.Text)
 return $ T.unpack inst
getString (AI to from) = atomically $ readTChan from

giveString :: Interface -> String -> IO ()
giveString (Regular h) s = hPutStrLn h s
giveString (Web conn) s = WS.sendTextData conn (T.pack s)
giveString (AI to from) s = atomically $ writeTChan to s
