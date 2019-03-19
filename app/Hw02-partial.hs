{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

{-# OPTIONS -fdefer-type-errors  #-}

{-# LANGUAGE RankNTypes #-}

module Main where
import qualified Data.Char  as Char
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import           Prelude    hiding (all, concat, reverse, takeWhile, zip)
import           Test.HUnit
import qualified Text.Read  as Read

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ -- testLists,
                               testWeather ] --,
--                               testSoccer ]
   return ()


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

