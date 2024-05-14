module Main where

import Data.Char (toLower)
import System.Process (system)
import System.Random (initStdGen)

import Data.List (isPrefixOf)
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

music :: IO ()
music = do
    _ <- system "abcmidi/midi2abc 'data/Mario Bros. - Super Mario Bros. Theme.mid' > /tmp/asd.abc"
    file <- readFile "/tmp/asd.abc"
    seed <- initStdGen
    let song = parseABC file
    let generated_track = take 40 $ generateRandom seed (head (notes song)) $ createModel $ notes song
    writeFile "gen.abc" $ composeABC song{notes = generated_track}
    _ <- system "abcmidi/abc2midi gen.abc -o /tmp/gen1.mid"
    _ <- system "timidity /tmp/gen1.mid"
    return ()

text :: IO ()
text = do
    file <- readFile "data/bbc_data.csv"
    seed <- initStdGen
    print $ unwords $ take 100 $ generateRandom seed "the us" $ createModel $ tokenizeText file

main :: IO ()
main = text
