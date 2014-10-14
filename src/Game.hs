
module Game where

import Random
import Data
import Player hiding (tell,put,get,putS,getS,putC,getC,putCS,getCS)
import GameState
import RoundEnd

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.State hiding (get, put)
import Control.Monad.Reader 
import Control.Exception (catch, Exception, throw, throwIO)
import Prelude hiding (catch)
import Data.List (sort)
import Data.Maybe (catMaybes, maybe, fromMaybe, isJust, fromJust)
import Data.Typeable (Typeable)




--the overall game-running functions

runGame :: Game ()
runGame = do
 broadcast "Enter 'ready' to begin. Use 'help' or '?' at any time if you want help."
 waitUntilAllReady
 broadcast "\nThe match is about to begin!\nPlease welcome our illustrious competitors:\n\n"
 gs <- gets id
 broadcast $ show gs
 broadcast "\n :::: MAY THE TEAM WITH THE SILLIEST NAME WIN! ::::\n"
 roundLoop

roundLoop = do
 eachPlayer_ (put (midRound.you) False)
 put phase PreRound
 [a,b,c,d] <- lift newHands
 gs <- gets id
 liftIO $ mapConcurrently (\f -> runStateT f gs) [deal a, nextPlayer >> deal b, nextPlayer >> nextPlayer >> deal c,
                                nextPlayer >> nextPlayer >> nextPlayer >> deal d]
 put phase Passing
 broadcast $ "Use 'give left 4', 'give partner A#' etc. to indicate your passing intentions.\n" ++
    "Then enter 'ready' to show you are done."
 waitAndThenPass
 findMahjong
 eachPlayer_ (put (midRound.you) True)
 put phase Playing
 sCatch playLoop resolveScore 
 broadcast $ "\n\nThe scores stand at:\n"
 showScores
 sAtom clearRound
 broadcast $ "Enter 'ready' to start the next round."
 waitUntilAllReady
 roundLoop


-- the loops that handle the round itself

playLoop :: Game () 
playLoop = do
 openTrick >>= trickLoop
 playLoop 

openTrick :: Game Int
openTrick = do
 sAtom $ do
  out <- checkIfOut
  if out then nextInPlayer 0 >> return () else return ()
 tell You "It is your turn.\nYou may open with any type of play."
 p <- doPlay
 outCheck
 case p of
  Play OnlyDogs [Dogs] -> do nextPlayer >> nextPlayer
                             nm' <- gets (name.you)
                             broadcast $ "Play passes across to " ++ nm' ++ "."
                             put inPlay []
                             openTrick
  Play Single [Mahjong] -> mayWish >> mapStateT atomically (nextInPlayer 0)
  Play (Straight _) (Mahjong:_) -> mayWish >> mapStateT atomically (nextInPlayer 0)
  _ -> mapStateT atomically (nextInPlayer 0)


trickLoop :: Int -> Game ()
trickLoop 4 = do 
 p <- trickRace countdown bombListeners
 if p == Play Single [] 
  then winsTrick 
  else do outCheck
          m <- sAtom $ nextInPlayer 0
          trickLoop m
trickLoop n = do 
 tell You $ "It is your turn. " ++ show n
 p <- trickRace doPlay bombListeners
 outCheck 
 m <- sAtom $ nextInPlayer $ if p == Play Single [] then n else 0
 trickLoop m

bombListeners = tRaceList [nextPlayer >> doBomb,
                           nextPlayer >> nextPlayer >> doBomb,
                           nextPlayer >> nextPlayer >> nextPlayer >> doBomb]




-- the two play-making functions


doPlay :: Game Play
doPlay = do
 put (playBlock.you) False
 p <- sAtom $ do
  p <- getCS (playChan.you)
  hnd <- getS (hand.you)
  inP <- getS inPlay
  let mq = if null inP then Nothing else Just $ head inP
  mw <- getS wish
  if couldUse hnd mq mw && not (p `uses` fromJust mw)
   then refusePlay p "There is a wish in force, you must comply with it!"
   else do
    if isJust mw && p `uses` fromJust mw then putS wish Nothing else return ()
    case (mq, valid p, p) of
     (Nothing,True,_) -> play p
     (Nothing,_,Play Single []) -> refusePlay p "You can't pass now."
     (Just _,_,Play Single []) -> pass
     (Just (Play Single [Dragon]),_,Play Single [Phoenix _]) -> refusePlay p "Only a bomb can beat the dragon."
     (Just (Play Single [c]),_,Play Single [Phoenix _]) -> play (Play Single [Phoenix $ faceValue c]) 
     (Just q@(Play qShape qCds),True,Play Bomb pCds) -> if p > q then play p else refusePlay p "You'll need a better bomb than that."
     (Just q@(Play qShape qCds),True,Play pShape pCds) -> case (qShape == pShape, p > q) of
        (False,_) -> refusePlay p "That is the wrong shape!"
        (True,False) -> refusePlay p "You'll need a stronger play."
        (True,True) -> play p
     _ -> throw (error "Invalid play passed to channel." :: IOError)
 if p == Play Double []
  then doPlay
  else return p

play :: Play -> StateT GameState STM Play
play p = do
 nm <- gets (name.you)
 inP <- getS inPlay
 broadcastS $ nm ++ " plays " ++ show p
 putS inPlay (p:inP)
 return p

pass = do 
 nm <- gets (name.you)
 broadcastS $ nm ++ " passes."
 return $ Play Single []

refusePlay :: Play -> String -> StateT GameState STM Play
refusePlay p s = do
 tellS You s
 withYou $ putInHand $ cards p
 return $ Play Double []

doBomb :: Game Play
doBomb = do
 put (playBlock.you) False
 p <- sAtom $ do
  p <- getCS (playChan.you)
  inP <- getS inPlay
  case (p, valid p && p > head inP) of
   (Play Bomb cs, True) -> do nm <- gets (name.you)
                              broadcastS $ nm ++ " interrupts with a bomb! " ++ show p
                              putS inPlay (p:inP)
                              return p
   (Play Bomb cs, False) -> refusePlay p "You'll need a better bomb than that!"
   _ -> do refusePlay p "It isn't your turn."
 if p == Play Double []
  then doBomb
  else return p



-- useful functions for during a round

countdown :: Game Play
countdown = do
  nm <- gets (name.you)
  broadcast $ nm ++ " is about to win the trick."
  liftIO (threadDelay 1000000) 
  broadcast $ "3"
  liftIO (threadDelay 1000000) 
  broadcast $ "2"
  liftIO (threadDelay 1000000) 
  broadcast $ "1"
  liftIO (threadDelay 1000000) 
  eachPlayer $ put (playBlock.you) True
  return $ Play Single []

winsTrick :: Game ()
winsTrick = do
   nm <- gets (name.you)
   broadcast $ nm ++ " has won the trick,"
   ps <- get inPlay
   let extra = sum (map (score.cards) ps)
   broadcast $ "which is worth " ++ show extra ++ " points."
   if head ps == Play Single [Dragon] 
    then do
     broadcast $ nm ++ " must choose which player to give these points to."
     put (request.you) DragonPass
     recipient <- givePointsLoop
     put (request.you) None
     broadcast $ nm ++ " chose to give the points to " ++ name recipient ++ "."
     scoreSoFar <- get (const (winnings recipient))
     put (const (winnings recipient)) $ scoreSoFar + extra
    else do 
     scoreSoFar <- get (winnings.you)
     put (winnings.you) $ scoreSoFar + extra 
   put inPlay []


outCheck :: Game ()
outCheck = do
  out <- sAtom checkIfOut
  pl <- gets you
  if out
   then do mFirstOut <- get firstOut
           case mFirstOut of
            Just q -> do broadcast $ name pl ++ " is out."
                         goneOut <- eachPlayer $ sAtom checkIfOut
                         if length (filter id goneOut) == 3 
                          then do broadcast "All but one player is out, so..."
                                  winsTrick
                                  sAtom $ nextInPlayer 0
                                  nm <- gets (name.you)
                                  broadcast $ nm ++ " has lost this round."
                                  liftIO $ throwIO AllButOne
                          else do ptnr <- gets partner
                                  if q == ptnr
                                   then do broadcast "That makes this a one-two victory!"
                                           liftIO $ throwIO OneTwo
                                   else return ()
            Nothing -> do broadcast $ name pl ++ " is first out, so wins this round!"
                          put firstOut (Just pl)  
                          return ()        
   else return ()




-- functions for requests and responses


mayWish = do
 broadcast "A wish may be made."
 put (request.you) Wish
 mWishCard <- wishRequestLoop
 put (request.you) None
 put wish mWishCard
 broadcast $ case mWishCard of
  Just fv -> "The wish is for " ++ show fv ++ "."
  Nothing -> "No wish has been made."

wishRequestLoop = do 
 tell You "What wish will you make? Name a card or enter 'none'."
 r <- getC (response.you)
 case r of
  WishFor v -> return $ Just v 
  NoWish -> return Nothing
  _ -> tell You "Je ne comprends pas." >> wishRequestLoop


givePointsLoop :: Game Player
givePointsLoop = do
 tell You "To whom will you give these points? (left/right)"
 r <- getC (response.you)
 case r of
  PassToLeft -> gets left 
  PassToRight -> gets right
  _ -> tell You "You must choose left or right." >> givePointsLoop



-- useful pre-round functions

findPlayer :: (Monad m) => Player -> StateT GameState m ()
findPlayer p = do
 current <- gets you
 if p == current then return () else nextPlayer >> findPlayer p

findMahjong = do
 hnd <- get (hand.you)
 if Mahjong `elem` hnd
  then do nm <- gets (name.you)
          broadcast $ nm ++ " has the Mahjong and hence the initiative."
          tell You "You must begin this round."
  else nextPlayer >> findMahjong

newDeck :: IO [TichuCard]
newDeck = runStd $ shuffle tichuDeck

newHands :: IO [([TichuCard],[TichuCard])]
newHands = do
 d <- newDeck
 return $ [(b 1 8 d,b 9 14 d),(b 15 22 d,b 23 28 d),(b 29 36 d,b 37 42 d),(b 43 50 d,b 51 56 d)]
    where b x y d = take (y+1-x) $ drop (x-1) d

deal :: ([TichuCard], [TichuCard]) -> Game ()
deal (cs,cs') = do
 withYouAtom $ putInHand cs
 tell You $ "Your first 8 cards: " ++ show (sort cs)
 tell You "Enter ready to get your other 6 cards."
 withYou $ do waitUntilReady
              rAtom $ putInHand cs'
 tell You $ "Your hand: " ++ show (sort $ cs ++ cs') 

waitUntilAllReady :: Game ()
waitUntilAllReady = mapStateT atomically $ do
 go <- allReady
 if go
  then eachPlayer_ $ putS (ready.you) False
  else lift retry

allReady = do
 readys <- eachPlayer $ getS (ready . you)
 return $ and readys


-- functions handling the pass

waitAndThenPass :: Game ()
waitAndThenPass = do
 (a,b,c,d) <- mapStateT atomically $ do
  allR <- allReady
  mPasses <- sequence $ [ getS (f . d) | d <- [you,right,partner,left], f <- [toRight,toPartner,toLeft]] 
  let passes = catMaybes mPasses
  if allR && length passes == 12 
   then do sequence_ $ replicate 4 $ putS (ready.you) False >> nextPlayer
           sequence_ $ [ putS (f . d) Nothing | d <- [you,right,partner,left], f <- [toRight,toPartner,toLeft]]
           let (a,b,c,d) = (take 3 passes, take 3 (drop 3 passes), take 3 (drop 6 passes), take 3 (drop 9 passes))
           sequence_ $ map (\x -> passThree x >> nextPlayer) [a,b,c,d]
           return (a,b,c,d)
   else lift retry
 nms <- sequence $ replicate 4 (do nm <- gets (name.you)
                                   nextPlayer
                                   return nm)
 sequence_ $ zipWith (\x from -> tellPassThree x from >> nextPlayer) [a,b,c,d] nms

tellPassThree ls@[_,_,_] from = nextPlayer >> (sequence_ $ map (\x -> tellPass x from >> nextPlayer) ls)
tellPass c nm = tell You $ nm ++ " passed you " ++ show c ++ "."

passThree :: [TichuCard] -> StateT GameState STM ()
passThree ls@[_,_,_] = nextPlayer >> (sequence_ $ map (\x -> passTo x >> nextPlayer) ls)

passTo :: TichuCard -> StateT GameState STM ()
passTo c = do
 hnd <- getS (hand.you)
 putS (hand.you) $ sort (c:hnd)

