{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "crypto"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
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
