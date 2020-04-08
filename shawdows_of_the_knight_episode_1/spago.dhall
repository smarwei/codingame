{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "shadown_of_the_knight"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "math"
  , "psci-support"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
