--Team Members:
----------------------
--  1.- Merino Almaraz, Roberto Carlos
--      merino@live.com
--  2.- Villazon Valdez, Jose Pablo
--      p.villazon@gmail.com
--  
--  Version 1.2
--  Added Transpose, CountSub, Soccer functions
--  
-------------------------------

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

{-# OPTIONS -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, zip, reverse, concat)
import Test.HUnit
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Text.Read as Read


main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testLists,
                               testWeather,
                               testSoccer 
                               ]
   return ()

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [ tintersperse,
    tinvert,
    ttranspose, 
    tconcat,
    tcountSub
    ]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

inter             :: a -> [a] -> [a]
inter _   []      = []
inter sep (x:xs)  = x : addChar sep xs

addChar            :: a -> [a] -> [a]
addChar _   []     = []
addChar charSep (x:xs) = charSep : x : addChar charSep xs

tintersperse :: Test
tintersperse = inter ',' "abcde" ~?= "a,b,c,d,e" 
--tintersperse = TestCase(assertEqual "intersperse, " ("a,b,c,d,e")(inter ',' "abcde"))
--tintersperse = "intersperse" ~: (assertFailure "testcase for intersperse" :: Assertion)
 

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

invert :: [(a,b)] -> [(b,a)]
invert [] = []
invert (x:xs) = swap x : invert xs

tinvert :: Test
tinvert = invert [("a",1),("a",2), ("a",3)] ~?= [(1,"a"),(2,"a"),(3,"a")]

 

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

conc :: [[a]] -> [a]
conc [] = []
conc (x:xs) = x ++ conc(xs) 


tconcat :: Test
tconcat = conc [[1,2,3], [4,5],[5,6,7]] ~?= [1,2,3,4,5,5,6,7]
--tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)


-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--    transpose  [[]] returns []
-- transpose is defined in Data.List

merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = [x,y] : (merge xs ys)

transpose [] = []
transpose [_] = []
transpose (x:y:xs) = merge x y ++ transpose xs


ttranspose :: Test
--ttranspose = "transpose" ~: (assertFailure "testcase for transpose" :: Assertion)
ttranspose = transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]]

-- countSub sub str

-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

countsub :: String -> String -> Int
countsub sub = length . Maybe.catMaybes . map (List.stripPrefix sub) . List.tails

tcountSub :: Test
tcountSub = countsub "aa" "aaa" ~?= 2 
--tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)

--------------------------------------------------------------------------------

-- Part One: Hottest Day

-- / Applies a function to the value in a Maybe if the value exists
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just $ f x
-- / Applies a function to the value in two Maybes if both values exist
maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f (Just x) (Just y) = Just $ f x y
maybeMap2 _ _        _        = Nothing

wProcessData :: Maybe (String,String,String) -> Maybe (Int, String)
wProcessData (Just (day,high,low)) =
   maybeMap (\x -> (x,day)) (maybeMap2 (\x y -> abs (x-y)) (readInt high) (readInt low))
wProcessData _ = Nothing

wExtractor :: [String] -> Maybe (String,String,String)
wExtractor (day:high:low:_) = Just (day,high,low)
wExtractor _                = Nothing

-- weather :: String -> String
weather = dropWhile (==' ') .
          snd .
          minimum .
          Maybe.mapMaybe (wProcessData . wExtractor . words) .
          drop 18 .
          lines
 

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "jul17.dat"
  putStrLn (weather str)

readInt :: String -> Maybe Int
readInt = Read.readMaybe

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "jul17.dat"
                              weather str @?= "6"

--------

-- Part Two: Soccer League Table

-- / Applies a function to the value in a Maybe if the value exists
maybeMaps :: (a -> b) -> Maybe a -> Maybe b
maybeMaps _ Nothing  = Nothing
maybeMaps f (Just x) = Just $ f x

-- / Applies a function to the value in two Maybes if both values exist
maybeMap2s :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2s f (Just x) (Just y) = Just $ f x y
maybeMap2s _ _        _        = Nothing

wsProcessData :: Maybe (String,String,String,String,String,String,String,String,String) -> Maybe (Int, String)
wsProcessData (Just (n,t,p,w,l,d,f,da,a)) =
   maybeMaps (\x -> (x,t)) (maybeMap2s (\x y -> abs (x-y)) (readInt f) (readInt a))
wsProcessData _ = Nothing

wsExtractor :: [String] -> Maybe (String,String,String,String,String,String,String,String,String)
wsExtractor (n:t:p:w:l:da:f:d:a:_) = Just (n,t,p,w,l,d,f,da,a)
wsExtractor _                = Nothing

goals = dropWhile (==' ') .
          snd .
          minimum .
          Maybe.mapMaybe (wsProcessData . wsExtractor . words) .
          drop 1 .
          lines


soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "soccer.dat"
  putStrLn (goals str)


readString :: String -> Maybe String
readString = Read.readMaybe

testSoccer :: Test
testSoccer = "goals" ~: do
  str <- readFile "soccer.dat"
  goals str @?= "Aston_Villa"

-- Part Three: DRY Fusion

weather2 :: String -> String
weather2 = dropWhile (==' ') .
          snd .
          minimum .
          Maybe.mapMaybe (wProcessData . wExtractor . words) .
          drop 18 .
          lines

soccer2 :: String -> String
soccer2 = dropWhile (==' ') .
          snd .
          minimum .
          Maybe.mapMaybe (wsProcessData . wsExtractor . words) .
          drop 1 .
          lines

