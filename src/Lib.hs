module Lib
    ( someFunc
    , tasks
    ) where

import qualified D01
import qualified D02
import qualified D03
import qualified D04
import qualified D05

tasks :: [IO ()]
tasks = [ D01.run
        , D02.run
        , D03.run
        , D04.run
        , D05.run
        ]

someFunc :: IO ()
someFunc = putStrLn "hello"
