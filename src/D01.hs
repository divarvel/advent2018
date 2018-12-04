{-# LANGUAGE OverloadedStrings #-}
module D01 where

import qualified Data.Set     as S
import           Data.Text    (Text, lines, replace, unpack)
import           Data.Text.IO (readFile)
import           Prelude      hiding (lines, readFile)
import           Text.Read    (readMaybe)

readFreqs :: FilePath -> IO (Maybe [Int])
readFreqs = fmap parseFreqs . readFile

parseFreqs :: Text -> Maybe [Int]
parseFreqs = traverse parseLine . lines

parseLine :: Text -> Maybe Int
parseLine = readMaybe . unpack . replace "+" ""

consecutive :: [Int] -> [Int]
consecutive = scanl (+) 0

firstDup :: [Int] -> Maybe Int
firstDup = go S.empty
  where
      go _ []     = Nothing
      go s (x:xs) =
          if x `S.member` s
              then Just x
              else go (S.insert x s) xs

getDupFreq :: [Int] -> Maybe Int
getDupFreq = firstDup . consecutive . cycle

run :: IO ()
run = do
    freqs <- readFreqs "./data/d01.txt"
    let err = putStrLn "Error, couldn't parse freqs"
    let displayTotal total = putStrLn $ "Total: " <> show total
    maybe err (displayTotal . sum) freqs
    let displayDupFreq freq = putStrLn $ "Duplicate frequence: " <> show freq
    maybe err displayDupFreq (getDupFreq =<< freqs)
