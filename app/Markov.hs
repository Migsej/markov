module Markov where

import Control.Arrow (Arrow (second))
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import System.Random (Random (random), RandomGen (split))

type Model a = Map.HashMap a (Map.HashMap a Float)
type FrequencyModel a = Map.HashMap a (Map.HashMap a Int)

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

generateRandom :: ((RandomGen g), (Hashable a), (Show a)) => g -> a -> FrequencyModel a -> [a]
generateRandom rand start model = start : generateRandom new_rand choice model
  where
    possible_states = case Map.lookup start model of
        Just x -> x
        Nothing -> error $ "no outgoing state " ++ show start ++ "in model"
    (new_rand, choice) = pick (Map.toList possible_states) rand

generate :: (Hashable a) => a -> Model a -> [a]
generate start model = choice : generate choice model
  where
    choice = choose (model Map.! start)
    choose :: Map.HashMap a Float -> a
    choose states = fst $ foldl (\(acc_state, acc_prob) (x_state, x_prob) -> if acc_prob > x_prob then (acc_state, acc_prob) else (x_state, x_prob)) (head state_list) state_list
      where
        state_list = Map.toList states

createModel :: (Hashable a) => [a] -> FrequencyModel a
createModel states = addMembers states Map.empty

combine :: (Hashable a) => FrequencyModel a -> FrequencyModel a -> FrequencyModel a
combine = Map.unionWith $ Map.unionWith (+)

addMembers :: (Hashable a) => [a] -> FrequencyModel a -> FrequencyModel a
addMembers (cur : next : rest) model = combine (addMember cur next model) (addMembers (next : rest) model)
addMembers _ model = model

addMember :: (Hashable a) => a -> a -> FrequencyModel a -> FrequencyModel a
addMember cur next = Map.insertWith insert cur (Map.singleton next 1)
  where
    insert states = Map.insertWith (+) key val
      where
        (key, val) = head $ Map.toList states
