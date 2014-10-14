module GameState where

import Player hiding (tell,put,get,putS,getS,putC,getC,putCS,getCS)
import Data
import NameGenerator

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State hiding (get, put)
import Control.Exception (catch, Exception, throw, throwIO)
import Prelude hiding (catch)


data GameState = GameState { you, left, partner, right :: Player
                           , inPlay :: TVar [Play] 
                           , yourScore, oppScore :: TVar Int
                           , yourName, oppName :: String
                           , phase :: TVar Phase
                           , firstOut :: TVar (Maybe Player)
                           , wish :: TVar (Maybe FaceValue)
                           }
instance Show GameState where
 show GameState {yourName = yn, oppName = on,
         you = y, left = l, partner = p, right = r} 
    = "Team " ++ yn ++ " (" ++ show y ++ ", " ++ show p ++ ")\n\n      VS.\n\n" ++ 
            "Team " ++ on ++ " (" ++ show l ++ ", " ++ show r ++ ")\n"  


initialiseGame :: Player -> Player -> Player -> Player -> IO GameState
initialiseGame y l p r = do
 n <- getName
 m <- getName
 atomically $ do
  a <- newTVar []
  [b,c] <- sequence [newTVar 0, newTVar 0]
  d <- newTVar PreRound
  e <- newTVar Nothing
  f <- newTVar Nothing
  return $ GameState y l p r a b c n m d e f


type Game a = StateT GameState IO a  

data Phase = PreRound | Passing | Playing | PostGame 
 deriving (Show, Read, Eq)

data Direction = You | L | Partner | R
 deriving (Show, Read, Eq)
onRotate You = L
onRotate L = Partner
onRotate Partner = R
onRotate R = You

seat :: Direction -> GameState -> Player
seat You = you
seat L = left
seat Partner = partner
seat R = right

rotate :: GameState -> GameState
rotate (GameState y l p r a b c m n d e f) = GameState r y l p a c b n m d e f

nextPlayer :: (Monad m) => StateT GameState m ()
nextPlayer = modify rotate

nextInPlayer :: Int -> StateT GameState STM Int
nextInPlayer 4 = return 4
nextInPlayer n = do
 nextPlayer
 out <- checkIfOut
 if out then nextInPlayer (n+1) else return (n+1)

--helpful StateT functions

getS :: (r -> TVar a) -> StateT r STM a
getS f = gets f >>= \a -> lift $ readTVar a
putS :: (r -> TVar a) -> a -> StateT r STM ()
putS f x = gets f >>= \a -> lift $ writeTVar a x
getCS :: (r -> TChan a) -> StateT r STM a
getCS f = gets f >>= \a -> lift $ readTChan a
putCS :: (r -> TChan a) -> a -> StateT r STM ()
putCS f x = gets f >>= \a -> lift $ writeTChan a x
get :: (r -> TVar a) -> StateT r IO a
get f = gets f >>= \a -> liftIO $ atomically $ readTVar a
put :: (r -> TVar a) -> a -> StateT r IO ()
put f x = gets f >>= \a -> liftIO $ atomically $ writeTVar a x
getC :: (r -> TChan a) -> StateT r IO a
getC f = gets f >>= \a -> liftIO $ atomically $ readTChan a
putC :: (r -> TChan a) -> a -> StateT r IO ()
putC f x = gets f >>= \a -> liftIO $ atomically $ writeTChan a x


trickRace :: Game a -> Game a -> Game a
trickRace a b = do
 r <- gets id
 ei <- liftIO $ race (runStateT a r) (runStateT b r)
 case ei of
  Left (a,s) -> modify (const s) >> return a
  Right (a,s) -> modify (const s) >> return a
  
tRaceList :: [Game a] -> Game a
tRaceList = foldr trickRace endless


endless :: Game a
endless = liftIO $ atomically retry

sCatch :: (Exception e) => Game a -> (e -> Game a) -> Game a
sCatch act handler = do
 s <- gets id
 (a,s') <- liftIO $ catch (runStateT act s) (\e -> runStateT (handler e) s)
 modify $ const s'
 return a

sAtom :: StateT GameState STM a -> Game a
sAtom = mapStateT atomically

withYou :: (Monad m) => ReaderT Player m a -> StateT GameState m a
withYou rdr = do
 pl <- gets you
 lift $ runReaderT rdr pl

withYouAtom :: ReaderT Player STM a -> Game a
withYouAtom rdr = sAtom $ withYou rdr




checkIfOut :: StateT GameState STM Bool
checkIfOut = do
 pl <- gets you
 cds <- lift $ runReaderT totalCards pl
 noPlay <- lift $ isEmptyTChan (playChan pl)
 return $ cds == 0 && noPlay

broadcast :: String -> Game ()
broadcast s = mapStateT atomically $ eachPlayer_ $ tellS You s
broadcastS s = eachPlayer_ $ tellS You s

tell :: Direction -> String -> Game ()
tell d s = putC (toChan . (seat d)) s
tellS d s = putCS (toChan . (seat d)) s

eachPlayer_ :: (Monad m) => StateT GameState m () -> StateT GameState m ()
eachPlayer_ act = sequence_ $ replicate 4 $ act >> nextPlayer

eachPlayer :: (Monad m) => StateT GameState m a -> StateT GameState m [a]
eachPlayer act = sequence $ replicate 4 $ nextPlayer >> act

