{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "vinidium"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "js-date"
  , "math"
  , "ordered-collections"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "../lib/**/*.purs" ]
}
