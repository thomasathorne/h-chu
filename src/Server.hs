module Server where

import Client (kick)
import Player (Player(..), totalCards)
import Game
import GameState

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.State (evalStateT)
import Control.Monad.Reader (runReaderT)


runServer :: Player -> Player -> Player -> Player -> IO ()
runServer n e s w = do 
  gs <- initialiseGame n e s w
--  putStrLn $ show gs
  raceList [loop, (evalStateT runGame gs), 
     chat n e s w, chat e s w n, chat s w n e, chat w n e s] 
 where 
  loop = do
    l <- getLine 
    case words l of 
     ("quit":_) -> return ()
     ("kick":a:_) -> case (pFromName a) of
                      (Just p) -> kick p >> loop
                      Nothing -> putStrLn "Not a valid seat to kick." >> loop
     _ -> putStrLn "Don't understand." >> loop
   where pFromName :: String -> Maybe Player
         pFromName a = case a of
                        "north" -> Just n
                        "east" -> Just e
                        "south" -> Just s
                        "west" -> Just w
                        _ -> Nothing


raceList = foldr race_ endless
 where endless = atomically retry

chat :: Player -> Player -> Player -> Player -> IO ()
chat a b c d = do
 atomically $ do
  s <- readTChan (fromChan a)
  out <- case s of
   "TICHU" -> return $ Just $ name a ++ " has called tichu!"
   "GRAND TICHU" -> return $ Just $ name a ++ " has called grand tichu!"
   "REQUEST tichus" -> do
      bt <- readTVar $ tichu b
      ct <- readTVar $ tichu c
      dt <- readTVar $ tichu d
      at <- readTVar $ tichu a
      writeTChan (toChan a) $ "The following amounts are bet on this round:\n" 
        ++ name b ++ ": " ++ show bt ++ "    "
        ++ name c ++ ": " ++ show ct ++ "    "
        ++ name d ++ ": " ++ show dt ++ "    "
        ++ "(you: " ++ show at ++ ")"
      return Nothing
   "REQUEST cards" -> do
      bcds <- runReaderT totalCards b
      ccds <- runReaderT totalCards c
      dcds <- runReaderT totalCards d
      writeTChan (toChan a) $ "The other players have the following remaining cards:\n" 
        ++ name b ++ ": " ++ show bcds ++ "    "
        ++ name c ++ ": " ++ show ccds ++ "    "
        ++ name d ++ ": " ++ show dcds ++ "    "
      return Nothing
   _ -> return $ Just $ name a ++ ": " ++ s
  case out of
   Nothing -> return ()
   Just o -> do
    writeTChan (toChan b) o
    writeTChan (toChan c) o
    writeTChan (toChan d) o
 chat a b c d



