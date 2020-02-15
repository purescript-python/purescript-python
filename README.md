# purescript-python


## Motivation

This is the successor to my works of Idris-Python.

Idris didn't provide me metadata such as source code positions hence debugging(especially FFI) is horrible,
but PureScript's IR output is pretty nice.

Besides, Idris FFI, though designed smart, but to be honest not that "industrially" practical, while
PureScript's Haskell-like `foreign import` mechanism is extremely straightforward and flexible.

Then I found PureScript, and now I think I love it.

## Status

After slightly modifying a JavaScript-like IR produced by the builtin compiler,
PureScript gets compiled to [`PySExpr`](https://github.com/thautwarm/PySExpr) and shall work since Python 3.5.

The code generation itself is finished, but still needs testing.

Currently I'm working on the support of some FFI libraries, for example, `Prelude`. Without `Prelude`
we cannot even print anything or proceed testing.
