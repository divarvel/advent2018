{-# LANGUAGE RecordWildCards #-}
module D03 where

import           Data.Attoparsec.Text
import           Data.Function        (on)
import           Data.List            (group, sort)
import qualified Data.List            as L
import           Data.Text            (Text, lines)
import           Data.Text.IO         (readFile)
import           Prelude              hiding (lines, readFile)

data Claim = Claim
  { _id    :: Int
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  }
  deriving (Eq)

instance Show Claim where
  show Claim{..} =
      "#" <> show _id <> " @ " <>
             show left <> "," <> show top <> ": " <>
             show width <> "x" <> show height

type Point = (Int, Int)

right :: Claim -> Int
right c = left c + width c - 1

bottom :: Claim -> Int
bottom c = top c + height c - 1

points :: Claim -> [(Int, Int)]
points c = do
    x <- [left c .. right c]
    y <- [top c .. bottom c]
    pure (x, y)

countOverlaps :: [Claim] -> Int
countOverlaps claims =
    let allPoints = points =<< claims
        groups = group . sort $ allPoints
        withOverlaps = filter ((> 1) . length) groups
    in length withOverlaps

overlaps :: Claim -> Claim -> Bool
overlaps c1 c2 =
    let noOverlapX =
          left c1 > right c2 ||
          left c2 > right c1
        noOverlapY =
          top c1 > bottom c2 ||
          top c2 > bottom c1
    in not $ noOverlapX || noOverlapY

withoutOverlaps :: [Claim] -> Maybe Claim
withoutOverlaps claims =
    let others c = filter (((/=) `on` _id) c) claims
    in L.find (\c -> not (any (overlaps c) (others c))) claims

claimParser :: Parser Claim
claimParser = do
    _id    <- char '#' *> decimal <* space
    left   <- char '@' *> space *> decimal
    top    <- char ',' *> decimal
    width  <- char ':' *> space *> decimal
    height <- char 'x' *> decimal
    pure Claim {..}

parseClaim :: Text -> Maybe Claim
parseClaim = either (const Nothing) Just . parseOnly claimParser

readClaims :: FilePath -> IO (Maybe [Claim])
readClaims = fmap (traverse parseClaim . lines) . readFile

run :: IO ()
run = do
    claims <- readClaims "./data/d03.txt"
    --print . fmap countOverlaps $ claims
    print . fmap withoutOverlaps $ claims
