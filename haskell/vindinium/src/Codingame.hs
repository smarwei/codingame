module Codingame
    ( bundle
    ) where

import Codingame.WebServices
import Codingame.SourcePackager
import Language.Haskell.Exts

import BotRunner
import Player
import Debug


sourcePath = "src/Player.hs"

parseMode :: ParseMode
parseMode = ParseMode {
        parseFilename = "<unknown>.hs",
        baseLanguage = Haskell2010,
        extensions = [EnableExtension ScopedTypeVariables, EnableExtension LambdaCase, EnableExtension MultiWayIf],
        ignoreLanguagePragmas = False,
        ignoreLinePragmas = True,
        fixities = Just preludeFixities,
        ignoreFunctionArity = False
        }
 
bundle :: IO ()
bundle = do
    source <- createMonolithicSourceWithMode parseMode sourcePath
    credentials <- readCredentials "credentials.json"

    putStrLn source

    let file = "Bundled.hs" 
    writeFile file $ "{-# LANGUAGE ScopedTypeVariables, LambdaCase, MultiWayIf #-}\n" ++ source

