module NameGenerator where

import Network
import System.IO
import Data.List



getName :: IO String
getName = do
  h <- connectTo "www.urbandictionary.com" (PortNumber 80)
  hSetBuffering h LineBuffering
  hPutStr h "GET /random.php HTTP/1.1\r\nHost: www.urbandictionary.com\r\n\r\n"
  contents <- searchLines h
  let term = findTerm contents
  hClose h
  return term


searchLines :: Handle -> IO String
searchLines h = do
  l <- hGetLine h
  if "<html><body>" `isPrefixOf` l
   then return l
   else searchLines h     


findTerm :: String -> String
findTerm [] = []
findTerm ('t':'e':'r':'m':'=':s) = replaces (takeWhile (/='\"') s) [("+"," "),("%27","'"),("%24","$"),("%23","#")]
findTerm (_:rest) = findTerm rest

replace :: (Eq a) => [a] -> ([a],[a]) -> [a]
replace [] _ = []
replace ls@(a:s) (x,y) | x `isPrefixOf` ls = y ++ replace ((drop $ length x) ls) (x,y)
                       | otherwise = a:replace s (x,y)

replaces :: (Eq a) => [a] -> [([a],[a])] -> [a]
replaces = foldl' replace

