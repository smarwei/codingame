{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "code_royal"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "js-date"
  , "math"
  , "ordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
