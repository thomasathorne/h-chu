module Player where

import Data

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader
import System.IO (Handle, hPutStrLn, hGetLine)
import Data.List (sort)
import Data.Maybe (catMaybes)


data Player = Player { handle :: TVar (Maybe Handle)
                     , playChan :: TChan Play
                     , fromChan, toChan :: TChan String
                     , hand :: TVar [TichuCard]
                     , toLeft, toRight, toPartner :: TVar (Maybe TichuCard)
                     , ready, midRound, playBlock :: TVar Bool
                     , tichu, winnings :: TVar Int
                     , request :: TVar Request
                     , response :: TChan Response
                     , name :: String }
instance Eq Player where
 a == b = name a == name b
instance Show Player where
 show p = name p

data Request = None | DragonPass | Wish deriving (Eq,Show)
data Response = WishFor FaceValue | NoWish | PassToLeft | PassToRight 

initialisePlayer :: String -> IO Player
initialisePlayer nm = atomically $ do
 a <- newTVar Nothing
 b <- newTChan 
 c <- newTChan
 d <- newTChan
 e <- newTVar []
 [f,g,h] <- sequence $ replicate 3 $ newTVar Nothing
 [i,j,k] <- sequence [newTVar False, newTVar False, newTVar True]
 l <- newTVar 0
 m <- newTVar 0
 n <- newTVar None
 o <- newTChan
 return $ Player a b c d e f g h i j k l m n o nm

getS :: (r -> TVar a) -> ReaderT r STM a
getS f = ask >>= \a -> lift $ readTVar $ f a
putS :: (r -> TVar a) -> a -> ReaderT r STM ()
putS f x = ask >>= \a -> lift $ writeTVar (f a) x
getCS :: (r -> TChan a) -> ReaderT r STM a
getCS f = ask >>= \a -> lift $ readTChan $ f a
putCS :: (r -> TChan a) -> a -> ReaderT r STM ()
putCS f x = ask >>= \a -> lift $ writeTChan (f a) x
get :: (r -> TVar a) -> ReaderT r IO a
get f = ask >>= \a -> liftIO $ atomically $ readTVar $ f a
put :: (r -> TVar a) -> a -> ReaderT r IO ()
put f x = ask >>= \a -> liftIO $ atomically $ writeTVar (f a) x
getC :: (r -> TChan a) -> ReaderT r IO a
getC f = ask >>= \a -> liftIO $ atomically $ readTChan $ f a
putC :: (r -> TChan a) -> a -> ReaderT r IO ()
putC f x = ask >>= \a -> liftIO $ atomically $ writeTChan (f a) x

rRace_ :: ReaderT r IO a -> ReaderT r IO b -> ReaderT r IO ()
rRace_ a b = do
 r <- ask
 liftIO $ race_ (runReaderT a r) (runReaderT b r)



showHand = do hnd <- get hand
              tell $ "Your hand: " ++ show hnd


say :: String -> ReaderT Player IO ()
say s = putC fromChan s

getL :: ReaderT Player IO String
getL = do
 h <- rAtom getHandle
 liftIO $ hGetLine h 

tell :: String -> ReaderT Player IO ()
tell s = do
 h <- rAtom getHandle
 liftIO $ hPutStrLn h s

getHandle :: ReaderT Player STM Handle
getHandle = do
 mb <- getS handle
 case mb of
  Just h -> return h
  Nothing -> lift retry 

totalCards :: ReaderT Player STM Int
totalCards = do
 unplay
 hnd <- getS hand 
 [a,b,c] <- sequence $ map (\f -> getS f >>=
   \m -> if m == Nothing then return 0 else return 1) [toLeft,toRight,toPartner]
 return $ sum [length hnd,a,b,c]

tidyUp :: ReaderT Player STM ()
tidyUp = do
 unplay
 gs <- sequence $ map getS [toLeft,toRight,toPartner]
 let gives = catMaybes gs
 sequence_ $ map (\f -> putS f Nothing) [toLeft,toRight,toPartner]
 putInHand gives



waitUntilReady :: ReaderT Player IO ()
waitUntilReady = rAtom $ do
 r <- getS ready
 if r then putS ready False else lift retry


putInHand :: [TichuCard] -> ReaderT Player STM ()
putInHand cs = do
 hnd <- getS hand
 putS hand (resetPhoenix $ sort $ hnd ++ cs)
 

unplay :: ReaderT Player STM ()
unplay = do
 pl <- ask
 emp <- lift $ isEmptyTChan $ playChan pl
 if emp 
  then return ()
  else do
   p <- getCS playChan
   putInHand $ cards p
   unplay

clearUp :: ReaderT Player STM ()
clearUp = do
 tidyUp
 putS hand []
 putS tichu 0
 putS winnings 0


-- helpful monadic juggling

rAtom :: ReaderT r STM a -> ReaderT r IO a
rAtom = mapReaderT atomically


