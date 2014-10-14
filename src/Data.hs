module Data where

import Data.List hiding (find)
import Data.Char (isDigit)

--basic card type and the list of the whole deck

data TichuCard = Dogs | Mahjong
               | TichuCard TichuSuit FaceValue 
               | Phoenix FaceValue | Dragon
 deriving (Read, Eq, Ord)
instance Show TichuCard where
 show (TichuCard s f) = show f ++ show s
 show Dogs = "Dogs"
 show Mahjong = "1"
 show Dragon = "Dragon"
 show (Phoenix v) = "Phoenix"
data TichuSuit = Jade | Daggers | Stars | Pagodas
 deriving (Read, Eq)
instance Ord TichuSuit where
 compare _ _ = EQ
instance Show TichuSuit where
 show Jade = "@"
 show Daggers = "!"
 show Stars = "*"
 show Pagodas = "#"
data FaceValue = Numeral Int | Jack | Queen | King | Ace
 deriving (Read, Eq, Ord)
instance Show FaceValue where
 show (Numeral n) = show n
 show Jack = "J"
 show Queen = "Q"
 show King = "K"
 show Ace = "A"
instance Enum FaceValue where
 toEnum 11 = Jack
 toEnum 12 = Queen
 toEnum 13 = King
 toEnum 14 = Ace
 toEnum n = Numeral n 
 fromEnum fv = value $ TichuCard Jade fv 

tichuDeck :: [TichuCard]
tichuDeck = [Dogs, Mahjong] ++
            do f <- map Numeral [2..10] ++ [Jack, Queen, King, Ace]
               s <- [Jade, Daggers, Stars, Pagodas]
               return $ TichuCard s f
            ++ [Phoenix (Numeral 1), Dragon]

parseSuit :: String -> Maybe TichuSuit
parseSuit "@" = Just Jade
parseSuit "!" = Just Daggers
parseSuit "#" = Just Pagodas
parseSuit "*" = Just Stars
parseSuit _ = Nothing

parseCard :: String -> Maybe TichuCard
parseCard "Dogs" = Just Dogs
parseCard "dogs" = Just Dogs
parseCard "Dragon" = Just Dragon
parseCard "dragon" = Just Dragon
parseCard "1" = Just Mahjong
parseCard "Phoenix" = Just (Phoenix (Numeral 1))
parseCard "phoenix" = Just (Phoenix (Numeral 1))
parseCard "p" = Just (Phoenix (Numeral 1))
parseCard ('p':s) = do {v <- parseValue s; return (Phoenix v)}
parseCard ('1':'0':n) = do {s <- parseSuit n; return (TichuCard s (Numeral 10))}
parseCard (d:n) = if d `elem` "23456789JQKAjqkaTt" 
                   then do {s <- parseSuit n; v <- parseValue [d]; return $ TichuCard s v}
                   else Nothing
parseCard _ = Nothing

parseValue :: String -> Maybe FaceValue
parseValue "A" = Just Ace
parseValue "K" = Just King
parseValue "Q" = Just Queen
parseValue "J" = Just Jack
parseValue "10" = Just (Numeral 10)
parseValue "a" = Just Ace
parseValue "k" = Just King
parseValue "q" = Just Queen
parseValue "j" = Just Jack
parseValue "T" = Just (Numeral 10)
parseValue "t" = Just (Numeral 10)
parseValue (d:[]) = if isDigit d && d /= '1' then Just (Numeral $ read [d]) else Nothing
parseValue _ = Nothing

data CardDescription = SpecificCard TichuCard | AnyOfValue FaceValue deriving (Show, Eq)

parseCardDescription :: String -> Maybe CardDescription
parseCardDescription s = case parseCard s of
                           Just c -> Just (SpecificCard c)
                           Nothing -> case parseValue s of
                                        Just v -> Just (AnyOfValue v)
                                        Nothing -> Nothing

--Int value of a single card

value :: TichuCard -> Int
value Dogs = 0
value Mahjong = 1
value (TichuCard _ v) | Numeral a <- v = a
                      | Jack == v = 11
                      | Queen == v = 12
                      | King == v = 13
                      | Ace == v = 14
value (Phoenix v) = value (TichuCard undefined v)
value Dragon = 15

valueOrd :: TichuCard -> TichuCard -> Ordering
valueOrd c d = value c `compare` value d

--non-exhaustive suit function

suit :: TichuCard -> TichuSuit
suit (TichuCard s _) = s

--types of different card combinations that can be played

data Shape = OnlyDogs | Single | Double | Triple | FullHouse
           | Straight Int | Steps Int | Bomb
 deriving (Show, Read, Eq)
data Play = Play { shape :: Shape, cards :: [TichuCard] }
 deriving (Read, Eq)
instance Show Play where
 show p = show $ cards p
 
--to check if a play is valid

valid :: Play -> Bool
valid (Play _ []) = False
valid (Play OnlyDogs ls) | ls == [Dogs] = True
                         | otherwise = False
valid (Play Single [c]) | c == Dogs = False
                        | otherwise = True
valid (Play Single _) = False
valid (Play Double [c,d]) | a <- value c, not (a `elem` [0,1,15]) = a == value d
valid (Play Double _) = False
valid (Play Triple [c,d,e]) | a <- value c, a == value d = a == value e 
valid (Play Triple _) = False
valid (Play FullHouse [c,d,e,f,g]) | value c /= value f
                                   , valid (Play Triple [c,d,e]) = valid (Play Double [f,g])
valid (Play FullHouse _) = False
valid (Play (Straight n) (Mahjong:rest@(a:as))) | n >= 5, value a == 2 
                                                , length rest == n-1
                                                , value (last as) <= 14 = consecutive $ map value rest
valid (Play (Straight n) ls@(a:as)) | n >= 5, value a >= 2
                                    , length ls == n
                                    , value (last as) <= 14 = consecutive $ map value ls
valid (Play (Straight _) _) = False
valid (Play (Steps n) ls) | n >= 2, ps <- pairs ls
                          , length ps == n 
                          , as@(a:rest) <- map head ps
                          , value a >= 2
                          , value (last rest) <= 14
                          , consecutive $ map value as = all doubles ps
 where doubles p = valid (Play Double p)  
       pairs [] = []
       pairs (a:[]) = [[a]]
       pairs (a:b:rest) = [a,b]:(pairs rest)
valid (Play (Steps _) _) = False
valid (Play Bomb cs@[c,d,e,f]) | noPhoenix cs, a <- value c
                               , a == value d, a == value e = a == value f
valid (Play Bomb cs@(c:_)) | noPhoenix cs, l <- length cs, l >= 5
                           , c /= Mahjong
                           , valid (Play (Straight l) cs) = allEq $ map suit cs
 where allEq (c:cs) = null $ filter (/= c) cs
valid (Play Bomb _) = False

noPhoenix = null . (filter (`elem` (map Phoenix $ map Numeral [1..10] ++ [Jack, Queen, King, Ace])))

consecutive :: [Int] -> Bool
consecutive [] = True
consecutive (a:[]) = True
consecutive (a:rest@(b:ls)) = a+1 == b && consecutive rest


-- To find sufficient legal plays of a given shape, using a given face value.
-- 'Sufficient' means: find at least one member of each equivalence class where
--          - we don't care about suits.
--          - we don't care about the difference between a normal card and
--            the phoenix pretending to be a card of that face value.
-- This is sufficient in the sense that it is good enough to implement wish enforcement.

find :: [TichuCard] -> Shape -> [Play]
find cs Single = filter valid $ map (\c -> Play Single [c]) cs
find cs Double = filter valid $ case extractPhoenix cs of
 (True, cs') -> map (\c -> Play Double [c, Phoenix (faceValue c)]) cs ++ find cs' Double
 (False, _) -> map (Play Double) pairs where pairs = map (take 2) $ tails cs
find cs Triple = filter valid $ case extractPhoenix cs of
 (True, cs') -> map (\(Play _ [c,d]) -> Play Triple [c, Phoenix (faceValue c), d]) (cs' `find` Double) ++ cs' `find` Triple
 (False, _) -> map (Play Triple) threes where threes = map (take 3) $ tails cs
find cs FullHouse = filter valid $ do 
   as <- cs `find` Triple
   bs <- cs `find` Double
   ds <- filter (\es -> length (filter (== Phoenix (Numeral 1)) (resetPhoenix es)) <= 1) [cards as ++ cards bs]
   [Play FullHouse ds]
find cs (Straight n) = filter valid $ case extractPhoenix cs of
 (True, cs') -> map (Play (Straight n)) $ (cs' `findStraights` n) ++ withPhoenix 
   where withPhoenix = do
          m <- [0..(n-1)]
          a <- cs' `findStraights` m
          b <- cs' `findStraights` (n-m-1)
          case b of 
           [] -> [a ++ [Phoenix (succ.faceValue.last $ a)]]
           _ -> [a ++ (Phoenix (pred.faceValue.head $ b)):b] 
 (False, _) -> map (Play (Straight n)) $ cs `findStraights` n
find cs (Steps n) = filter valid $ do
  stps <- (cs `find` Double) `findSteps` n
  stps' <- [concat $ map cards stps]
  stps'' <- filter (\es -> length (filter (== Phoenix (Numeral 1)) (resetPhoenix es)) <= 1) [stps']
  [Play (Steps n) stps'']
find cs Bomb = filter valid $ map (Play Bomb) $ fours ++ straightFlushes
 where fours = map (take 4) $ tails cs
       straightFlushes = do
        s <- [Jade,Stars,Pagodas,Daggers] 
        concat $ map (\n -> (filter (`ofSuit` s) cs) `findStraights` n) [5..13]

-- this function calls 'nubBy' and hence only returns one straight in each equivalence class
findStraights :: [TichuCard] -> Int -> [[TichuCard]]
findStraights cs 0 = [[]]
findStraights cs n | cs' <- nubBy (\a b -> value a == value b) cs = do 
 st <- findStraights cs' (n-1)
 c <- cs'
 filter (consecutive . (map value)) [c:st]

findSteps :: [Play] -> Int -> [[Play]]
findSteps dbls 0 = [[]]
findSteps dbls n = do
  stps <- findSteps dbls (n-1)
  dbl <- dbls
  filter (consecutive . (map value')) [dbl:stps]
 where value' (Play _ (c:_)) = value c

extractPhoenix :: [TichuCard] -> (Bool,[TichuCard])
extractPhoenix cs | Phoenix (Numeral 1) `elem` cs = (True, fst (takeOneOut (== Phoenix (Numeral 1)) cs))
                  | otherwise = (False, cs)

ofSuit :: TichuCard -> TichuSuit -> Bool
ofSuit (TichuCard s _) t = s == t
ofSuit _ _ = False

ofFaceValue :: TichuCard -> FaceValue -> Bool
ofFaceValue (TichuCard _ v) w = v == w
ofFaceValue _ _ = False

subsets 0 ls = [[]]
subsets n ls = do a <- [1..length ls]  
                  bs <- subsets (n-1) (drop a ls)
                  return $ (ls !! (a-1)):bs

-- to see if the wish is possible

couldUse :: [TichuCard] -> Maybe Play -> Maybe FaceValue -> Bool
couldUse hnd (Just p@(Play shp cs)) (Just fv) = not.null $ do
 play <- filter (`uses` fv) (hnd `find` shp) ++ filter (`uses` fv) (hnd `find` Bomb)
 filter (> p) [play]
couldUse hnd Nothing (Just fv) = not.null $ filter (`ofFaceValue` fv) hnd
couldUse _ _ _ = False

uses :: Play -> FaceValue -> Bool
uses (Play _ cs) fv = not.null $ filter (`ofFaceValue` fv) cs

-- to reorder a list of cards to try and make them a valid play

findValid :: [TichuCard] -> [Play]
findValid [] = []
findValid (Dogs:[]) = [Play OnlyDogs [Dogs]]
findValid (c:[]) = [Play Single [c]]
findValid cs | [TichuCard _ fv,_] <- sort cs = filter valid [Play Double (setPhoenix fv cs)]
             | [TichuCard _ fv,_,_] <- sort cs = filter valid [Play Triple (setPhoenix fv cs)]
             | cs'@[TichuCard _ fva,_,TichuCard _ fvb,_] <- sort cs = filter valid $ [Play Bomb cs] ++ do
                                                              fv <- [fva,fvb]
                                                              return $ Play (Steps 2) (sortBy valueOrd $ setPhoenix fv cs)
             | cs'@[TichuCard _ fvb,_,_,TichuCard _ fvt,_] <- sort cs 
             , straights <- do fv <- filter (\v -> not $ any (`ofValue` v) cs) [succ fvt, fvt .. pred fvb]
                               return $ Play (Straight 5) (sortBy valueOrd $ setPhoenix fv cs)
             , fullhouses <- do fv <- [fvt, fvb]
                                cs'' <- [sortBy valueOrd $ setPhoenix fv cs] 
                                map (Play FullHouse) [reverse cs'', cs'']
             = filter valid $ [Play Bomb cs'] ++ straights ++ fullhouses
             | l <- length cs, even l, cs' <- sort cs, fvb <- faceValue (head cs') 
             , straights <- do fv <- reverse $ take 2 $ filter (\v -> not $ any (`ofValue` v) cs) [pred fvb ..]
                               return $ Play (Straight l) (sortBy valueOrd $ setPhoenix fv cs)
             , steps <- do fv <- nub $ map faceValue cs
                           return $ Play (Steps $ l `div` 2) (sortBy valueOrd $ setPhoenix fv cs)
             = filter valid $ [Play Bomb cs'] ++ straights ++ steps 
             | l <- length cs, cs' <- sort cs, fvb <- faceValue (head cs') 
             , straights <- do fv <- reverse $ take 2 $ filter (\v -> not $ any (`ofValue` v) cs) [pred fvb ..]
                               return $ Play (Straight l) (sortBy valueOrd $ setPhoenix fv cs)
             = filter valid $ [Play Bomb cs'] ++ straights 
             | otherwise = []

faceValue (TichuCard _ v) = v
faceValue (Phoenix v) = v
faceValue Dogs = Numeral 0
faceValue Mahjong = Numeral 1
faceValue Dragon = Numeral 0

--the all-important Ord instance

instance Ord Play where
 compare (Play Bomb (a:as)) (Play Bomb (b:bs)) | length as == length bs = compare (value a) (value b)
                                               | otherwise = compare (length as) (length bs)
 compare (Play Single [a]) (Play Single [b@(Phoenix v)]) | value a == value b = LT
                                                         | otherwise = compare (value a) (value b)
 compare (Play Single [a@(Phoenix v)]) (Play Single [b]) | value a == value b = GT
                                                         | otherwise = compare (value a) (value b)
 compare (Play _ (_:_)) (Play Bomb (_:_)) = LT
 compare (Play Bomb (_:_)) (Play _ (_:_)) = GT
 compare (Play _ (a:_)) (Play _ (b:_)) = compare (value a) (value b)
 compare (Play _ _) (Play _ _) = EQ

--functions to count the points

points :: TichuCard -> Int
points (TichuCard _ King) = 10
points (TichuCard _ (Numeral 10)) = 10
points (TichuCard _ (Numeral 5)) = 5
points Dragon = 25
points (Phoenix _) = (-25)
points _ = 0

score :: [TichuCard] -> Int
score ls = sum $ map points ls 

--some helper functions

ofValue a v = value a == value (TichuCard Jade v)

takeOneOut :: (a -> Bool) -> [a] -> ([a],a)
takeOneOut p as = (takeWhile (not . p) as ++ tail (dropWhile (not . p) as), head $ filter p as)

resetPhoenix :: [TichuCard] -> [TichuCard]
resetPhoenix ls = map rp ls
 where rp (Phoenix _) = Phoenix (Numeral 1)
       rp c = c

setPhoenix :: FaceValue -> [TichuCard] -> [TichuCard]
setPhoenix fv ls = map sp ls
 where sp (Phoenix _) = Phoenix fv
       sp c = c

