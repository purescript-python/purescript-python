# Purescript JS FFI transfer and test summary

## purescript-strings

tests : `purescript-python/passing/test/String`

note tests for `Data.String.CodePoints` and `Data.String.Regex` is skipped.

FFI for these to modules is not finished yet.

## purescript-globals

tests : `purescript-python/passing/test/Globals.purs`

FFI for function `toPrescision`, `decodeURI` and `encodeURI` is not finished yet, thus tests for these functions is skipped.

## modules without tests

There is no tests for the following packages in their repos, thus no test for FFI of the following packages.

- `purescript-math`
- `purescript-random`
- `purescript-lazy`
- `purescript-assert`
- `purescript-functions`
- `purescript-exceptions` (TODO: verify implementation manually due to differences between Python `Exception`s and JS `Error`s)
- `purescript-effect`
- `purescript-console`

## packages with tests but not added to purescript-python/passing yet

- `purescript-enums`
- `purescript-prelude`
- `purescript-control`
- `purescript-foldable-traversable`
- `purescript-partial`
- `purescript-nullable`
- `purescript-refs`

## packages with FFI and tests

- `purescript-arrays`
- `purescript-integers`
- `purescript-quickcheck`
- `purescript-record`
- `purescript-st`
- `purescript-unfoldable`
- `purescript-unsafe-coerce`

## purescript-python only packages

- `purescript-show-python`