# purescript-python


## Motivation

This is the successor to my works of Idris-Python.

Idris didn't provide me metadata such as source code positions hence debugging(especially FFI) is horrible,
but PureScript's IR output is pretty nice.

Besides, Idris FFI, though designed smart, but to be honest not that "industrially" practical, while
PureScript's Haskell-like `foreign import` mechanism is extremely straightforward and flexible.

Then I found PureScript, and now I think I love it.

## Status

The code generation itself is finished, but still needs testing.

Currently I'm working on the support of some FFI libraries, for example, `Prelude`. Without `Prelude`
we cannot even print anything or proceed testing.

The first binary release(`v0.1`) will be available after
- passing the testcases provided by PureScript GitHub repository, and
- implementing an extensible and customizable Python FFI providing mechanism.

## Python Package Generation Specification

Generating PySExpr
-------------------------

After slightly modifying a JavaScript-like IR produced by the builtin compiler,
PureScript gets compiled to [`PySExpr`](https://github.com/thautwarm/PySExpr) and **shall work since Python 3.5**.

The reason why we generate the IR `PySExpr` instead of Python source code,
is for getting better cross-Python-version compatibility, Python-version-specific optimizations,
**source code positioning for using existing Python debuggers in PureScript**, and expression-first expressiveness. You could check out [this reddit post](https://www.reddit.com/r/ProgrammingLanguages/comments/f41odv/a_compiler_back_end_by_which_you_write) for more details.


Directory Tree of Generated Package
---------------------------------------------


Given a PureScript module, not losing the generality, we abbreviate it as `A.B`.

After processing this module via the command

```shell
# `output` is the directory produced by the PureScript build tool `spago`.
pspy --out-top aaa/bbb/output_top_dir --entry-corefn output/A.B/path_to_corefn.json
```

We read `CoreFn` from `.json` file and know we're generating PureScript module `A.B`, which shall be compiled from a source file whose path matches one of following patterns:
- source code from current project:

  `PROJECT_DIR/src/**/*.purs`

- source code from dependencies:

  `PROJECT_DIR/.spago/<package-name>/v<version>/src/**.*.purs`


Command `pspy` generates following directory tree(all `__init__.py` will be added later, but not in Haskell side):

```
- aaa/bbb/output_top_dir
    - A
        - B
            - pure.py
            - pure.src.py

    - ffi
- .pure-py-ffi
    - 

```

`pure.src.py` Generated for Each Module
-----------------------------------------------

This Python module creates Python code/bytecode object.

In CPython, every Python file will be compiled to a Python code object, which will finally be executed in
CPython virtual machine.

In `pure.src.py`, we just create the code object,
but don't execute it, for achieving the further flexibility of caching and composition of our compilation.

`pure.py` Generated for Each Module
----------------------------------------------

This is, actually the loader for corresponding `pure.src.py`.

This module implements the concrete code caching system which avoids the redundant Python source code to bytecode compilation, and finally greatly reduce the module loading time.

Hence, a PureScript module `A.B` compiled by PureScript-Python
will be able to imported by the statement `import output_top_dir.A.B.pure`.

The code of `pure.py`, corresponding to a PureScript module, is fixed to be

```python
from purescripto import LoadPureScript
__py__ = globals()
__ps__ = LoadPureScript(__file__, __name__)
__all__ = list(__ps__)
__py__.update(__ps__)
```

which relies on the Python package `purescripto`.

The Python package `purescripto` provides a common RTS and supplements all required functionalities for being a full featured PureScript backend.

<!-- 

`purescript_impl.src.py`
-----------------------------------------



<details>

<summary> This is an example of generated PySExpr </summary>

The command is `pspy-one-module --foreign-top ./src --out-top ./python --corefn .\output\\Main\corefn.json`

```python
# example purescript_impl.src.py
from py_sexpr.terms import *
from py_sexpr.stack_vm.emit import module_code
res = block( "No document"
           , assign( "$foreign"
                   , call( var('import_module')
                         , "python.Main.purescript_foreign" ) )
           , assign( "ps_Unit"
                   , block( define("ps_Unit", [], block(this))
                          , set_attr( var("ps_Unit")
                                    , "value"
                                    , new(var("ps_Unit")) ) ) )
           , assign( "ps_main"
                   , call( get_attr(var("$foreign"), "println")
                         , metadata(8, 16, "src\Main.purs", 1) ) )
           , assign( "exports"
                   , record( ("Unit", var("ps_Unit"))
                           , ("main", var("ps_main"))
                           , ( "println"
                             , get_attr(var("$foreign"), "println") ) ) ) )
# this is code object
res = module_code(res)
```


</details>


<details>

<summary> Above example is from this source code </summary>

```purescript
module Main where

data Effect a
data Unit = Unit

foreign import println :: forall a. a -> Effect Unit

main = println 1
```

</details>

`purescript_impl.py`
--------------------------

Conditionally load and execute the code made by `purescript_impl.src.py`, which results in a Python module `__ps__`.

If code object has been cached and hasn't been out of date,
the cached code object will be used, instead of invoking `purescript_impl.src.py`.


`__ps__` is the Python global variable `exports` in the hidden module made by `purescript_impl.src.py`,  with all export symbols bound.

Module `purescript_impl` will then bind all symbol names to its `__all__` and the corresponding values to its global dictionary.


The code of `purescript_impl.py` which provides above functionalities is fixed:

```python
from purescripto import LoadPureScript
__py__ = globals()
__ps__ = LoadPureScript(__file__, __name__)
__all__ = list(__ps__)
__py__.update(__ps__)
``` -->
