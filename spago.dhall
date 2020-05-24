{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "crypto"
  , "debug"
  , "effect"
  , "fixed-points"
  , "free"
  , "generics-rep"
  , "integers"
  , "memoize"
  , "node-readline"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "run"
  , "strings"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
