module Markov where

import Control.Arrow (Arrow (second))
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import System.Random (Random (random), RandomGen (split))

type Model a = Map.HashMap a (Map.HashMap a Int)

pick :: (RandomGen g) => [(a, Int)] -> g -> (g, a)
pick items seed = (snd (split seed), pick' (fst (random seed) * total) (map (second fromIntegral) items))
  where
    total :: Float
    total = fromIntegral $ sum $ map snd items

    pick' :: Float -> [(a, Float)] -> a
    pick' _ [] = error "epty list :("
    pick' r ((x, probability) : xs)
        | r < probability = x
        | otherwise = pick' (r - probability) xs

generateRandom :: ((RandomGen g), (Hashable a), (Show a)) => g -> a -> Model a -> [a]
generateRandom rand start model = start : generateRandom new_rand choice model
  where
    possible_states = case Map.lookup start model of
        Just x -> x
        Nothing -> error $ "no outgoing state " ++ show start ++ "in model"
    (new_rand, choice) = pick (Map.toList possible_states) rand

createModel :: (Hashable a) => [a] -> Model a
createModel states = addMembers states Map.empty

combine :: (Hashable a) => Model a -> Model a -> Model a
combine = Map.unionWith $ Map.unionWith (+)

addMembers :: (Hashable a) => [a] -> Model a -> Model a
addMembers (cur : next : rest) model = combine (addMember cur next model) (addMembers (next : rest) model)
addMembers _ model = model

addMember :: (Hashable a) => a -> a -> Model a -> Model a
addMember cur next = Map.insertWith insert cur (Map.singleton next 1)
  where
    insert states = Map.insertWith (+) key val
      where
        (key, val) = head $ Map.toList states
