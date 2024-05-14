module Markov where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import System.Random (Random (random), RandomGen (split))

type Model a num = Map.HashMap a (Map.HashMap a num)

pick :: (RandomGen g) => [(a, Float)] -> g -> (g, a)
pick items seed = (snd (split seed), pick' (fst (random seed)) items)
  where
    pick' :: Float -> [(a, Float)] -> a
    pick' _ [] = error "epty list :("
    pick' r ((x, probability) : xs)
        | r < probability = x
        | otherwise = pick' (r - probability) xs

generateRandom :: ((RandomGen g), (Hashable a), (Show a)) => g -> a -> Model a Float -> [a]
generateRandom rand start model = start : generateRandom new_rand choice model
  where
    possible_states = case Map.lookup start model of
        Just x -> x
        Nothing -> error $ "no outgoing state " ++ show start ++ "in model"
    (new_rand, choice) = pick (Map.toList possible_states) rand

generate :: (Hashable a) => a -> Model a Float -> [a]
generate start model = choice : generate choice model
  where
    choice = choose (model Map.! start)
    choose :: Map.HashMap a Float -> a
    choose states = fst $ foldl (\(acc_state, acc_prob) (x_state, x_prob) -> if acc_prob > x_prob then (acc_state, acc_prob) else (x_state, x_prob)) (head state_list) state_list
      where
        state_list = Map.toList states

createModel :: (Hashable a) => [a] -> Model a Float
createModel states = Map.map convert_probability $ addMembers states Map.empty
  where
    convert_probability :: (Hashable a) => Map.HashMap a Int -> Map.HashMap a Float
    convert_probability model = Map.fromList $ map (\(state, x) -> (state, fromIntegral x / total_entries :: Float)) model_list
      where
        model_list = Map.toList model
        total_entries = fromIntegral $ foldl (\acc (_, b) -> acc + b) 0 model_list

combine :: (Hashable a) => Model a Int -> Model a Int -> Model a Int
combine = Map.unionWith $ Map.unionWith (+)

addMembers :: (Hashable a) => [a] -> Model a Int -> Model a Int
addMembers (cur : next : rest) model = combine (addMember cur next model) (addMembers (next : rest) model)
addMembers _ model = model

addMember :: (Hashable a) => a -> a -> Model a Int -> Model a Int
addMember cur next = Map.insertWith insert cur (Map.singleton next 1)
  where
    insert states = Map.insertWith (+) key val
      where
        (key, val) = head $ Map.toList states
