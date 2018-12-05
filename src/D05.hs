{-# LANGUAGE OverloadedStrings #-}
module D05 where

import           Data.Bool    (bool)
import           Data.Char    (isLower, toLower, toUpper)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Text.IO (readFile)
import           Prelude      hiding (readFile)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

canFuse :: Char -> Char -> Bool
canFuse a b =
  let differentCase = isLower a `xor` isLower b
      sameLetter = toLower a == toLower b
  in sameLetter && differentCase

part1 :: Text -> Int
part1 = T.length . iterate' fuseOnce
  where
    iterate' f a = bool (iterate' f (f a)) a $ f a == a
    fuseOnce = T.foldl' go ""
    go acc c = case T.unsnoc acc of
      Nothing        -> T.singleton c
      Just (acc', a) -> bool (T.snoc acc c) acc' $ canFuse a c

clean :: Char -> Text -> Text
clean char = T.replace (T.singleton char) "" . T.replace (T.singleton $ toUpper char) ""

part2 :: Text -> Int
part2 polymer = minimum . fmap (part1 . (`clean` polymer)) $ ['a' .. 'z']

run :: IO ()
run = do
    polymer <- T.stripEnd <$> readFile "./data/d05.txt"
    print . part1 $ polymer
    print . part2 $ polymer
