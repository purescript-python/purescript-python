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


## Python Package Generation Specification


Directory Tree of Generated Package
---------------------------------------------


Given a purescript module `A.B`, not losing the generality, we abbreviate it as `A.B`.

After processing this module via the command

```shell
# `output` is the directory produced by the purescript build tool `spago`.
pspy --foreign-top xxx/yyy/foreign_dir --out-top aaa/bbb/output_top_dir --corefn output/A.B/path_to_corefn.json
```

We read `CoreFn` from `.json` file and know we're generating purescript module `A.B`.

If `A.B` used FFI directly, the directory tree of finally generated Python package is

```
- aaa/bbb/output_top_dir
    - A
        - __init__.py
        - B
            - __init__.py
            - purescript_impl.py
            - purescript_impl.src.py
            - purescript_foreign.py
```

where `purescript_foreign.py` is copied from `xxx/yyy/foreign_dir/purescript_foreign/A/B.py`.


If `A.B` didn't use FFI directly, the directory tree of finally generated Python package is

```
- aaa/bbb/output_top_dir
    - A
        - __init__.py
        - B
            - __init__.py
            - purescript_impl.py
            - purescript_impl.src.py
```

`purescript_impl.src.py`
-----------------------------------------

The generated `PySExpr` is in `purescript_impl.src.py`, following is an example:
```python
# example purescript_impl.src.py
from py_sexpr.terms import *
from py_sexpr.stack_vm.emit import module_code
res = block( "No document"
           , call(var('import_module'), ".foreign", var("__package__"))
           , assign( "ps_Unit"
                   , block( define("ps_Unit", [], block(this))
                          , set_attr( var("ps_Unit")
                                    , "value"
                                    , new(var("ps_Unit")) ) ) )
           ...)
res = module_code(res) # this is code object
```

`purescript_impl.py`
--------------------------

Conditionally load and execute the code made by `purescript_impl.src.py`, which results in a Python module `__ps__`.

If code object has been cached and hasn't been out of date,
the cached code object will be used, instead of invoking `purescript_impl.src.py`.


`__ps__` has a Python global `exports` with all export symbols bound,
module `purescript_impl` will bind all symbol names to its `__all__` and its global dictionary.


The code of `purescript_impl.py` which provides above functionalities is fixed:

```python
from purescripto import LoadPureScript
_py__ = globals()
_ps__ = LoadPureScript(__name__, __file__)
__all__ = list(__ps__.exports)
__py__.update(__ps__.exports)
```
