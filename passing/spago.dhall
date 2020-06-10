{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "globals"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "refs"
  , "st"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "pspy"
}
