module Main where

import Client
import Server
import Player

import System.IO
import System.IO.Error hiding (catch)
import System.Environment (getArgs)
import Network
import Text.Printf
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception hiding (handle)
import Control.Monad.Reader
import Prelude hiding (catch)


defaultPort :: Int
defaultPort = 4444


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> withSocketsDo $ do bracket (getSock defaultPort) (\s -> sClose s >> putStrLn "Socket closed.") useSock
    args -> withSocketsDo $ do let port = read (head args)
                               bracket (getSock port) (\s -> sClose s >> putStrLn "Socket closed.") useSock
 where getSock :: Int -> IO Socket
       getSock port = do 
         sock <- listenOn (PortNumber (fromIntegral port))
         printf "Listening on port %d\n" port
         return sock

useSock sock = do
 [n,e,s,w] <- mapM initialisePlayer ["north","east","south","west"]
 race_ (manageConnections n e s w sock) (runServer n e s w)



manageConnections n e s w sock = do
 h <- getConn
 concurrently (catch (manageClient h) 
    (\e -> if isEOFError e then putStrLn "A connection closed unexpectedly." else ioError e))
    (manageConnections n e s w sock)
 return ()
  where getConn = do
         (h, host, port) <- accept sock
         printf "Accepted connection from %s: %s\n" host (show port)
         hSetBuffering h LineBuffering
         hPutStrLn h "\n\n  -- Welcome to H-chu!! -- \n\n\n"
         return h
        manageClient h = do
         openSeats <- availableSeats n e s w
         hPutStrLn h $ "The following seats are open: " ++ show openSeats
         hPutStrLn h "Choose your seat, or type 'quit' to exit."
         l <- hGetLine h
         if l == "quit" 
          then hClose h >> putStrLn "Connection closed."
          else case words l of
                      ("north":_) -> tryLink h n
                      ("south":_) -> tryLink h s
                      ("east":_) -> tryLink h e
                      ("west":_) -> tryLink h w
                      _ -> hPutStrLn h "That seat isn't available." >> manageClient h
          

tryLink :: Handle -> Player -> IO ()
tryLink h p = do
 success <- atomically $ do pHandle <- readTVar $ handle p
                            case pHandle of
                             Nothing -> do writeTVar (handle p) (Just h)
                                           return True
                             Just _ -> do return False
 if success
  then do hPutStrLn h $ "\nYou have joined the game, occupying the " ++ name p ++ " seat."
          putStrLn $ "The " ++ name p ++ " seat is now occupied." 
          race_ (catch (runReaderT playerInterface p)
           (\e -> if isEOFError e 
                   then do putStrLn $ "Connection lost with " ++ name p ++ " seat." 
                           atomically $ writeTVar (handle p) Nothing
                   else ioError e))
           ((atomically $ do mb <- readTVar (handle p)
                             case mb of
                              Nothing -> return ()
                              _ -> retry) >> (putStrLn $ "Terminated connection with " ++ name p ++ " seat.")) 
  else hPutStrLn h "Sorry, that seat appears to be unavailable."

 

availableSeats n e s w = do
 valid <- sequence $ map playerConn [n,e,s,w] 
 return $ concat $ zipWith (\b s -> if b then [] else [s]) valid ["north","east","south","west"]

playerConn :: Player -> IO Bool
playerConn p = do 
 pHandle <- atomically $ readTVar $ handle p
 case pHandle of
  Nothing -> return False
  Just h -> return True

