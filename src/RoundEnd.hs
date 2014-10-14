
{-# LANGUAGE DeriveDataTypeable #-}


module RoundEnd where


import Data
import Player hiding (tell,put,get,putS,getS,putC,getC,putCS,getCS)
import GameState


import Control.Concurrent.STM
import Control.Monad.State hiding (get, put)
import Control.Monad.Reader 
import Control.Exception (Exception,throw)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)



data EndOfRound = OneTwo | AllButOne deriving (Show, Eq, Typeable)
instance Exception EndOfRound



clearRound :: StateT GameState STM ()
clearRound = do
 eachPlayer $ withYou clearUp
 putS inPlay []
 putS firstOut Nothing
 putS wish Nothing

showScores = do
 yN <- gets yourName
 oN <- gets oppName
 yScore <- get yourScore
 oScore <- get oppScore
 broadcast $ "Team " ++ yN ++ ": " ++ show yScore ++ " points.\n"
 broadcast $ "Team " ++ oN ++ ": " ++ show oScore ++ " points.\n" 

resolveScore :: EndOfRound -> Game ()
resolveScore OneTwo = mapStateT atomically $ do
 nextInPlayer 0
 nextPlayer
 yN <- gets yourName
 oppN <- gets oppName
 broadcastS $ "Team " ++ yN ++ " get 200 points for their one-two."
 score <- getS yourScore
 putS yourScore $ score + 200
 mf <- getS firstOut
 eachPlayer_ (resolveTichu $ fromMaybe (throw (error "No player marked as first out." :: IOError)) mf)
resolveScore AllButOne = mapStateT atomically $ do
 nextInPlayer 0
 yN <- gets yourName
 oppN <- gets oppName
 nm <- gets (name.you)
 youWins <- getS (winnings.you)
 mf <- getS firstOut
 let fo = fromMaybe (throw (error "No player marked as first out." :: IOError)) mf
 broadcastS $ nm ++ "'s winnings (" ++ show youWins ++ " points) go to " ++ name fo ++ " who was first out."
 foWins <- getS (const $ winnings fo)
 putS (const $ winnings fo) (foWins + youWins)
 leftWins <- getS (winnings.left)
 rightWins <- getS (winnings.right)
 partWins <- getS (winnings.partner)
 withYou tidyUp
 hnd <- getS (hand.you)
 let handScore = score hnd
 broadcastS $ nm ++ "'s remaining hand (" ++ show handScore ++ " points) goes to Team " ++ oppN ++ ".\n"
 broadcastS $ "This round Team " ++ yN ++ " scored " ++ show partWins ++ "."
 broadcastS $ "This round Team " ++ oppN ++ " scored " ++ show (leftWins + rightWins + handScore) ++ ".\n"
 yScore <- getS yourScore
 putS yourScore $ yScore + partWins
 oScore <- getS oppScore
 putS oppScore $ oScore + leftWins + rightWins + handScore
 eachPlayer_ (resolveTichu fo)
  

resolveTichu :: Player -> StateT GameState STM ()
resolveTichu fo = do
 pl <- gets you
 t <- getS (tichu.you)
 if t == 0 
  then return ()
  else if pl == fo then tichuPoints t else tichuPoints (-t)

tichuPoints :: Int -> StateT GameState STM ()
tichuPoints t = do
 nm <- gets (name.you)
 broadcastS $ nm ++ " gets " ++ show t ++ " points for his tichu call."
 score <- getS yourScore 
 putS yourScore $ score + t


