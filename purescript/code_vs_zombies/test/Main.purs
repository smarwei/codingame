module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
    let input = {
        x: 20,
        y: 20,
        width: 100,
        height: 100,
        turns: 50
    }
    -- loop input $ calcWindows input
    log "hi"