module D02 where

import           Control.Arrow ((&&&))
import           Data.List     (group, sort)
import           Data.Maybe    (mapMaybe)
import           Data.Text     (Text, lines, pack, unpack)
import           Data.Text.IO  (readFile)
import           Prelude       hiding (lines, readFile)

readIds :: FilePath -> IO [Text]
readIds = fmap lines . readFile

sameLetters :: Text -> (Bool,Bool)
sameLetters =
    let ls = fmap length . group . sort . unpack
        cs = elem 2 &&& elem 3
    in cs . ls

singleLetterDiff :: Text -> Text -> Maybe Text
singleLetterDiff a b =
    let pairs = zip (unpack a) (unpack b)
        bool c f t = if c then t else f
        df :: Eq a => (a, a) -> Int
        df (a', b') = bool (a' == b') 1 0
        dfs = df <$> pairs
    in if sum dfs == 1
      then Just . pack . fmap fst . filter ((== 0) . df) $ pairs
      else Nothing

getSame :: [Text] -> [Text]
getSame xs = do
  x1 <- xs
  mapMaybe (singleLetterDiff x1) xs

getCount :: [Text] -> (Int, Int)
getCount = foldr go (0,0) . fmap sameLetters
  where
    go (True,  True)  (twos, threes) = (twos + 1, threes + 1)
    go (True,  False) (twos, threes) = (twos + 1, threes    )
    go (False, True)  (twos, threes) = (twos,     threes + 1)
    go (False, False) (twos, threes) = (twos,     threes    )

run :: IO ()
run = do
    ids <- readIds "./data/d02.txt"
    print . uncurry (*) . getCount $ ids
    print $ getSame ids
