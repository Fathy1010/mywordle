{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Wordle (module Wordle) where

import           Data.List (delete, elemIndices)

data WordleAnswers = Correct | Misplaced | Incorrect
  deriving (Eq)

instance Show WordleAnswers where
  show :: WordleAnswers -> String
  show Correct   = "green"
  show Misplaced = "yellow"
  show Incorrect = "gray"

type InternalState = Int

-- | generate the next element in a pseudorandom sequence from the prevSeed
-- Taken from the Week 10 workshop!
nextRand :: InternalState -> InternalState
nextRand prevSeed = (a * prevSeed + c) `mod` m
  where
    -- Parameters for linear congruential RNG.
    a = 1664525
    c = 1013904223
    m = 2 ^ (32 :: Int)

-- | Read the "src/all_words.txt" file in to a list
-- This files contains all possible five letter words
-- wordle will accept
-- >>> length <$> validWords
-- 14855
validWords :: IO [String]
validWords = lines <$> readFile "src/all_words.txt"

-- | Read the "src/possible_answers.txt" file in to a list
-- This files contains all possible answers wordle may use
-- >>> length <$> validAnswers
-- 2310
validAnswers :: IO [String]
validAnswers = lines <$> readFile "src/possible_answers.txt"

-- | Generate a random Wordle answer
-- Do not use the current seed, but the next seed...
-- /Hint/: Use !! to index an array
-- >>> generateRandomAnswer 1
-- (1015568748,"halve")
-- >>> generateRandomAnswer 9223372036854775806
-- (1010575173,"reedy")

-- | Generate a random Wordle answer
generateRandomAnswer :: InternalState -> IO (InternalState, String)
generateRandomAnswer prevState = do
  answers <- validAnswers
  let newState = nextRand prevState
      idx = newState `mod` length answers
  return (newState, answers !! idx)

-- | Check if the user has guessed a valid word (a 5 letter word that is one of the validWords)
-- >>> checkValid "notValid"
-- False
-- >>> checkValid "hello"
-- True
-- >>> checkValid "waacs"
-- True
-- >>> checkValid "abcde"
-- False
checkValid :: String -> IO Bool
checkValid g = do
  words <- validWords
  return $ length g == 5 && g `elem` words

-- | Generate feedback for a Wordle guess compared to a target word.
-- This function compares two words of equal length letter by letter and returns
-- a list of WordleAnswers, which could be:
--   * `Correct`   -> The letter is in the correct position (green)
--   * `Misplaced` -> The letter is in the word but not in the correct position (yellow)
--   * `Incorrect` -> The letter is not in the word at all (gray)
--
-- The algorithm works in two passes:
-- 1. The first pass builds a list of "unmatched" letters in the target that have not yet been used.
-- 2. The second pass checks if the remaining letters in the guess are present in the
--    unmatched letters (Misplaced), or not present at all (Incorrect).
-- We need to keep track of the unmatched letters because we can't use the same letter in the target
-- more than once when marking a letter in the guess as Misplaced (yellow). See the
-- `makeFeedback "aaacc" "bbbaa"` test below for an example.
--
-- Feel free to do this yourself, it is a fun problem!
-- >>> makeFeedback "hello" "atoll"
-- [gray,gray,yellow,green,yellow]
-- >>> makeFeedback "clued" "queue"
-- [gray,gray,yellow,yellow,gray]
-- >>> makeFeedback "aaaaa" "abcde"
-- [green,gray,gray,gray,gray]
-- >>> makeFeedback "atlol" "goooy"
-- [gray,gray,gray,green,gray]
-- >>> makeFeedback "aaabb" "acada"
-- [green,yellow,green,gray,gray]
-- >>> makeFeedback "aaacc" "bbbaa"
-- [yellow,yellow,gray,gray,gray]
makeFeedback :: String -> String -> [WordleAnswers]
makeFeedback guess target = snd $ foldl f (unmatched, []) (zip guess target)
  where
    -- List of letters in target that are incorrect in the guess ('unmatched')
    unmatched = snd <$> filter (uncurry (/=)) (zip guess target)
    -- The fold function
    -- us: unmatched characters we haven't used yet
    -- answers: current list of WordleAnswers so far
    -- g: letter in guess
    -- t: letter in target
    f (us, answers) (g, t)
      -- If the letter was correct, return Correct
      | g == t      = (us,          answers ++ [Correct])
      -- If the letter is incorrect and we haven't used this character yet
      -- (it is still in the list of unmatched characters), return Misplaced (yellow)
      -- and remove it from the list of unmatched characters
      | g `elem` us = (delete g us, answers ++ [Misplaced])
      -- Otherwise, we have used this letter already, so it is Incorrect (grey)
      | otherwise   = (us,          answers ++ [Incorrect])

--   ___ ___                  .___    _____             .___
--  /   |   \_____ _______  __| _/   /     \   ____   __| _/____
-- /    ~    \__  \\_  __ \/ __ |   /  \ /  \ /  _ \ / __ |/ __ \
-- \    Y    // __ \|  | \/ /_/ |  /    Y    (  <_> ) /_/ \  ___/
--  \___|_  /(____  /__|  \____ |  \____|__  /\____/\____ |\___  >
--        \/      \/           \/          \/            \/    \/
-- In Hard Mode, when you receive clues such as green (correct position)
-- or yellow (correct letter, wrong position) for a letter in your guess,
-- your subsequent guesses must incorporate those clues

-- | All Correct Letters must be in consecutive guesses
-- >>> ensureGreens "slate" "slime" "slope"
-- WAS WAS True
-- WAS NOW Prelude.undefined
-- NOW True
-- >>> ensureGreens "slate" "plime" "slope"
-- WAS WAS False
-- WAS NOW Prelude.undefined
-- NOW False

-- Ensure all correct (green) letters are in the same position in subsequent guesses
ensureGreens :: String -> String -> String -> Bool
ensureGreens prev guess target = all correctOrUnchanged $ zip3 prev guess target
  where
    correctOrUnchanged (p, g, t)
      | p == t    = g == t  -- If the letter was correct in the previous guess, it must stay correct
      | otherwise = True    -- Otherwise, we don't care about its position

-- | Ensure that yellow letters are in the next guess (can be anywhere)
-- >>> ensureYellows "slate" "slime" "slope"
-- WAS WAS True
-- WAS NOW Prelude.undefined
-- NOW True
-- >>> ensureYellows "world" "dlrow" "orwld"
-- WAS WAS True
-- WAS NOW Prelude.undefined
-- NOW True
-- >>> ensureYellows "aorld" "xxxxx" "orwld"
-- WAS WAS False
-- WAS NOW Prelude.undefined
-- NOW False

-- | Ensure all misplaced (yellow) letters appear somewhere in the subsequent guess
ensureYellows :: String -> String -> String -> Bool
ensureYellows prev guess target = all (`elem` guess) misplacedLetters
  where
    feedback = makeFeedback prev target
    misplacedLetters = [p | (p, Misplaced) <- zip prev feedback]


-- | Make sure the guesses follows both ensureGreens and ensureYellows as well, as the guess not being the same as previous
-- >>> ensureCriteria Nothing "stove" "ready"
-- True
-- >>> ensureCriteria (Just "aorld") "xxxxx" "orwld"
-- WAS WAS False
-- WAS NOW True
-- NOW False
-- >>> ensureCriteria (Just "slate") "slime" "slope"
-- True
-- >>> ensureCriteria (Just "slate") "plime" "slope"
-- WAS WAS False
-- WAS NOW True
-- NOW False
-- >>> ensureCriteria (Just "slate") "slate" "slope"
-- WAS WAS False
-- WAS NOW True
-- NOW False


-- | Ensure the guess follows the rules: respects green and yellow clues, and is not the same as previous guess
ensureCriteria :: Maybe String -> String -> String -> Bool
ensureCriteria Nothing guess target = True  -- First guess doesn't need to follow previous rules
ensureCriteria (Just prev) guess target =
  guess /= prev && ensureGreens prev guess target && ensureYellows prev guess target
