{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Char (toLower)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import System.Process (system)
import System.Random (initStdGen)

import Markov (createModel, generateRandom)

group2 :: [String] -> [String]
group2 (x : y : rest) = unwords [x, y] : group2 rest
group2 _ = []

tokenizeText :: String -> [String]
tokenizeText = group2 . words . map toLower . filter (`notElem` "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~")

data ABC = ABC
    { header :: String
    , notes :: [String]
    }

parseABC :: String -> ABC
parseABC abcdata = ABC{header = parsed_header, notes = parsed_notes}
  where
    program_thing = "%%MIDI program"
    parts = break (isPrefixOf program_thing) $ lines abcdata
    parsed_header = unlines (fst parts) ++ program_thing ++ " 0\n"
    parsed_notes = words $ filter (`notElem` "\\|") $ unlines $ tail $ snd parts

composeABC :: ABC -> String
composeABC abc = header abc ++ unparsed_notes
  where
    unparsed_notes = unwords (notes abc) ++ "\n"

data Weather
    = Sunny
    | Rainy
    | Cloudy
    deriving (Ord, Eq, Show, Generic)

instance Hashable Weather

test :: IO ()
test = do
    seed <- initStdGen
    let test_data_set = Map.fromList [(Sunny, Map.fromList [(Sunny, 7), (Cloudy, 1), (Rainy, 2)]), (Rainy, Map.fromList [(Sunny, 1), (Cloudy, 3), (Rainy, 6)]), (Cloudy, Map.fromList [(Sunny, 2), (Cloudy, 3), (Rainy, 5)])]
    let amount = 1000000
    let result = fromlist $ map (\x -> fromIntegral x / fromIntegral amount) $ tolist $ foldl count (0, 0, 0) $ take amount $ generateRandom seed Sunny test_data_set :: (Float, Float, Float)
    print result
  where
    tolist :: (a, a, a) -> [a]
    tolist (a, b, c) = [a, b, c]

    fromlist :: [a] -> (a, a, a)
    fromlist (a : b : c : _) = (a, b, c)
    fromlist _ = error "ur mom"

    count :: (Int, Int, Int) -> Weather -> (Int, Int, Int)
    count (sunny, cloudy, rainy) Sunny = (sunny + 1, cloudy, rainy)
    count (sunny, cloudy, rainy) Cloudy = (sunny, cloudy + 1, rainy)
    count (sunny, cloudy, rainy) Rainy = (sunny, cloudy, rainy + 1)

music :: IO ()
music = do
    _ <- system "abcmidi/midi2abc 'data/Mario Bros. - Super Mario Bros. Theme.mid' > /tmp/asd.abc"
    file <- readFile "/tmp/asd.abc"
    seed <- initStdGen
    let song = parseABC file
    let generated_track = take 100 $ generateRandom seed (head (notes song)) $ createModel $ notes song
    writeFile "/tmp/gen.abc" $ composeABC song{notes = generated_track}
    _ <- system "abcmidi/abc2midi /tmp/gen.abc -o /tmp/gen1.mid"
    _ <- system "timidity /tmp/gen1.mid"
    return ()

text :: IO ()
text = do
    file <- readFile "data/bbc_data.csv"
    seed <- initStdGen
    print $ unwords $ take 100 $ generateRandom seed "denmark is" $ createModel $ tokenizeText file

main :: IO ()
main = text
