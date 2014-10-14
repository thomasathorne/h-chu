module Random (runStd, shuffle) where

import System.Random


data WithRandom g a = WithRandom (g -> (a,g))
runWithRandom :: g -> WithRandom g a -> (a,g)
runWithRandom g (WithRandom k) = k g
runStd :: WithRandom StdGen a -> IO a
runStd (WithRandom k) = getStdRandom k
instance RandomGen g => Functor (WithRandom g) where
 fmap f (WithRandom h) = WithRandom ((\(a,g) -> (f a, g)) . h) 
instance RandomGen g => Monad (WithRandom g) where
 return a = WithRandom (\g -> (a,g))
 (>>=) (WithRandom k) f = WithRandom ((\(a,g) -> runWithRandom g $ f a) . k)



die :: RandomGen g => Int -> WithRandom g Int
die n = WithRandom (randomR (1,n))
coin :: RandomGen g => WithRandom g Bool
coin = WithRandom random
uniform :: RandomGen g => WithRandom g Double
uniform = WithRandom random

prob :: RandomGen g => Double -> WithRandom g Bool
prob p = do u <- uniform
            return (u <= p)

sample :: (RandomGen g) => Int -> WithRandom g r -> WithRandom g [r]
sample samples pop = sequence $ take samples $ repeat pop

mean list = sum list / fromIntegral (length list)
centre list = map (\a -> a - mu) list where mu = mean list
variance list = (sum $ map (^2) $ centre list) / fromIntegral (length list - 1)

standardNormal :: RandomGen g => WithRandom g Double
standardNormal = do dat <- sequence $ replicate n coin
                    let s = sum $ map (\b -> if b then 1.0 else (-1.0)) dat
                    return (s / sqrt (fromIntegral n)) 
 where n = 1000


shuffle :: RandomGen g => [a] -> WithRandom g [a] 
shuffle [] = return []
shuffle deck = do next <- die (length deck)
                  rest <- shuffle (take (next - 1) deck ++ drop next deck)
                  return $ (deck !! (next-1)):rest


