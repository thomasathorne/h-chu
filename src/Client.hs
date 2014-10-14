
module Client where

import Data
import Player
import HelpText

import System.IO
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Cont (ContT, runContT, callCC)
import Control.Monad.Reader
import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (maybeToList, catMaybes, isNothing)


playerInterface :: ReaderT Player IO ()
playerInterface = rRace_ (runContT getter return) cListener

getter :: ContT r (ReaderT Player IO) ()
getter = callCC $ \abort -> do
 l <- lift getL
 r <- lift $ get request
 case r of
  DragonPass -> case words l of
   ("left":_) -> lift $ putC response PassToLeft
   ("right":_) -> lift $ putC response PassToRight
   _ -> lift $ tell "You must choose left or right."
  Wish -> lift $ case words l of
   ("none":_) -> putC response NoWish
   (s:_) -> case parseValue s of
             Just v -> putC response (WishFor v)
             Nothing -> tell "I don't understand."
   _ -> tell "I don't understand."
  None -> case words l of
   ("say":s) -> lift $ say (unwords s)
   ("quit":_) -> do lift $ tell "Quitting." 
                    mh <- lift $ get handle 
                    case mh of
                     Nothing -> abort ()
                     Just h -> do lift $ put handle Nothing
                                  lift $ lift $ hClose h
                                  abort ()
   ("ready":_) -> lift $ do 
                     put ready True
                     tell "Waiting for other players."
   ("unready":_) -> lift $ do 
                       put ready False
                       tell "Readiness cancelled."
   ("show":"hand":_) -> lift showHand
   ("show":"tichu":_) -> lift $ do 
                            t <- get tichu
                            tell $ "You have " ++ show t ++ " points bet on this round."
   ("show":"tichus":_) -> lift $ say "REQUEST tichus"
   ("show":"cards":_) -> lift $ say "REQUEST cards"
   ("show":"name":_) -> lift $ do 
                           p <- ask
                           tell $ "You are " ++ name p ++ "."
   ("show":"pass":_) -> lift showGives
   ("show":"winnings":_) -> lift $ do 
                               w <- get winnings
                               tell $ "You have " ++ show w ++ " points so far this round."
   ("show":_) -> lift showHand
   ("tichu":_) -> lift $ do 
                     n <- rAtom totalCards
                     case n of 
                      14 -> do put tichu 100 >> say "TICHU" >> tell "You have called tichu!"
                      8 -> do during <- get midRound 
                              if during 
                               then tell "Cannot call tichu now." 
                               else put tichu 200 >> say "GRAND TICHU" >> tell "You have called grand tichu!"
                      _ -> tell "Cannot call tichu now."
   ("clear":_) -> lift $ (rAtom tidyUp >> showHand)
   ("give":"left":s:_) -> lift $ (tryGive toLeft s >> showGives)
   ("give":"partner":s:_) -> lift $ (tryGive toPartner s >> showGives)
   ("give":"right":s:_) -> lift $ (tryGive toRight s >> showGives)
   ("ungive":"left":_) -> lift $ (unGive toLeft >> showGives)
   ("ungive":"partner":_) -> lift $ (unGive toPartner >> showGives)
   ("ungive":"right":_) -> lift $ (unGive toRight >> showGives)
   ("play":ls) -> let mcds = map parseCardDescription ls
         in lift $ if any isNothing mcds
                    then tell "I don't understand."
                    else do block <- get playBlock
                            if block then tell "You can't play now." else tryPlay $ catMaybes mcds 
   ("pass":_) -> lift $ do block <- get playBlock
                           if block then tell "You can't pass now." else putC playChan (Play Single [])
   ("unplay":_) -> lift $ (rAtom tidyUp >> showHand)
   ("help":_) -> lift $ tell helpText
   ("?":_) -> lift $ tell helpText
--------------------------------------------------for debugging purposes only!!
--   ("bomb":_) -> lift $ putC playChan (Play Bomb (map (TichuCard Jade) $ map Numeral [3..10]))
--   ("hide":_) -> lift $ put hand [TichuCard Stars Ace]
-------------------------------------------------------------------------------
   ls -> let mcds = map parseCardDescription ls
         in lift $ if any isNothing mcds
                    then tell "I don't understand."
                    else do block <- get playBlock
                            if block then tell "You can't play now." else tryPlay $ catMaybes mcds  
 getter


tryPlay :: [CardDescription] -> ReaderT Player IO ()
tryPlay ls = do
 mcs <- rAtom $ sequence $ map getCard ls
 if any isNothing mcs
  then do tell "You don't have all of those cards"
          rAtom $ putInHand $ catMaybes mcs
  else do let valids = findValid $ catMaybes mcs
          if null valids
           then do tell "That is not a valid play."
                   rAtom $ putInHand $ catMaybes mcs
           else putC playChan (head valids)

 
tryGive :: (Player -> TVar (Maybe TichuCard)) -> String -> ReaderT Player IO ()
tryGive d s = do
 hnd <- get hand
 case parseCardDescription s of
  Just cd -> do
    mc <- rAtom $ getCard cd
    case mc of
      Just c -> do unGive d
                   put d mc
      Nothing -> tell "You haven't got that card."
  Nothing -> tell "I don't understand."


unGive :: (Player -> TVar (Maybe TichuCard)) -> ReaderT Player IO ()
unGive d = do
 rAtom $ do
  m <- getS d
  case m of
   Nothing -> return ()
   Just c -> do hnd <- getS hand
                putS d Nothing
                putS hand (sort (c:hnd))


showGives :: ReaderT Player IO ()
showGives = do
 a <- get toLeft
 b <- get toPartner
 c <- get toRight
 tell $ "To left: " ++ show (maybeToList a) ++
        "    To partner: " ++ show (maybeToList b) ++
        "    To right: " ++ show (maybeToList c)




getCard :: CardDescription -> ReaderT Player STM (Maybe TichuCard)
getCard (SpecificCard c) = do
 hnd <- getS hand
 if c `elem` hnd 
  then do putS hand (fst $ takeOneOut (==c) hnd)
          return $ Just c
  else return Nothing
getCard (AnyOfValue v) = do
 hnd <- getS hand
 if null $ filter (`ofValue` v) hnd
  then return Nothing
  else let (hnd',c) = takeOneOut (`ofValue` v) hnd
       in do putS hand hnd'
             return $ Just c 



cListener :: ReaderT Player IO ()
cListener = do
 s <- getC toChan
 tell s
 cListener

kick :: Player -> IO ()
kick = runReaderT $ do
 mb <- get handle
 case mb of
  Nothing -> liftIO $ putStrLn "That seat is unoccupied."
  Just h -> do liftIO $ hPutStrLn h "The server is kicking you."
               put handle Nothing
               liftIO $ putStrLn "Kick successful."
               liftIO $ hClose h

