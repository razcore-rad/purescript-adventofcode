{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "crypto"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "open-memoize"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "strings"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
