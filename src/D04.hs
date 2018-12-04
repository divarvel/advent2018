{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module D04 where

import           Control.Applicative  ((<|>))
import           Control.Arrow        ((&&&))
import           Data.Attoparsec.Text
import           Data.Functor         (($>))
import           Data.List            (foldl', maximumBy, sort)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import           Data.Ord             (comparing)
import           Data.Text            (Text, lines)
import           Data.Text.IO         (readFile)
import           Data.Tuple           (swap)
import           Prelude              hiding (lines, readFile)

data Timestamp = Timestamp
  { year   :: Int
  , month  :: Int
  , day    :: Int
  , hour   :: Int
  , minute :: Int
  }
  deriving (Eq, Ord, Show)

newtype GuardId = GuardId { getGuardId :: Int }
  deriving (Eq, Ord, Show)

data Event =
    NewGuard GuardId
  | WakesUp
  | FallsAsleep
  deriving (Eq, Show, Ord)

type Line = (Timestamp, Event)

type Minute = Int
type Nap = (Minute, Minute)
type MinuteStats = (Minute, Int)
type SleepStats = (Map GuardId [Nap])
newtype Solution = Solution (GuardId, Minute)

instance Show Solution where
    show (Solution (GuardId gid, minute')) =
        "#" <> show gid <> " : " <> show minute' <>
        " (" <> show (gid * minute') <> ")"

handleWake :: GuardId -- Guard id
           -> Minute -- sleep start
           -> Minute -- now
           -> SleepStats -> SleepStats
handleWake guard start stop =
    let pair = (start, stop)
        func = maybe (Just [pair]) (Just . (pair:))
    in M.alter func guard

getSleepStats :: [Line] -> SleepStats
getSleepStats =
    let startingState = (Nothing, Nothing, M.empty)
        go (_, _, stats)                 (_, NewGuard gid) = (Just gid, Nothing, stats)
        go (Just gid, _, stats)          (ts, FallsAsleep) = (Just gid, Just $ minute ts, stats)
        go (Just gid, Just start, stats) (ts, WakesUp)     = (Just gid, Nothing, handleWake gid start (minute ts) stats)
        go st _ = st -- should not happen
        trd (_, _, a) = a
    in trd . foldl' go startingState


parseTimestamp :: Parser Timestamp
parseTimestamp = do
    year   <- char '[' *> decimal
    month  <- char '-' *> decimal
    day    <- char '-' *> decimal
    hour   <- space *> decimal
    minute <- char ':' *> decimal <* char ']' <* space
    pure Timestamp {..}

parseEvent :: Parser Event
parseEvent = parseNewGuard <|>
             string "falls asleep" $> FallsAsleep <|>
             string "wakes up" $> WakesUp
  where
    parseNewGuard = do
        _ <- string "Guard #"
        guard <- decimal
        _ <- string " begins shift"
        pure . NewGuard . GuardId $ guard

parseLine :: Parser Line
parseLine = (,) <$> parseTimestamp <*> parseEvent

psL :: Text -> Maybe Line
psL = either (const Nothing) Just . parseOnly parseLine

readLines :: FilePath -> IO (Maybe [Line])
readLines = fmap (traverse psL . lines) . readFile

mostSleepyGuard :: SleepStats -> (GuardId, [Nap])
mostSleepyGuard = maximumBy (comparing (sum' . snd)) . M.toList
  where
    sum' = sum . fmap (uncurry (-) . swap)

mostSleptMinute :: [Nap] -> MinuteStats
mostSleptMinute list =
  let mins = fmap (id &&& isAsleep) [0..59]
      isAsleep m = length $ filter (within m) list
      within m (start, end) = m >= start && m < end
  in maximumBy (comparing snd) mins

step1 :: SleepStats -> Solution
step1 = Solution . fmap (fst . mostSleptMinute) . mostSleepyGuard

step2 :: SleepStats -> Solution
step2 stats =
    let mostSleptMinuteByGuard = mostSleptMinute <$> stats
        timesSlept = snd . snd -- first access MinuteStats, then the the sleep count
        guardWithMostSleptMinute = maximumBy (comparing timesSlept) $ M.toList mostSleptMinuteByGuard
    in Solution . fmap fst $ guardWithMostSleptMinute

run :: IO ()
run = do
  lns <- fmap sort <$> readLines "./data/d04.txt"
  let lns' = fromJust lns
  let stats = getSleepStats lns'
  print $ step1 stats
  print $ step2 stats
