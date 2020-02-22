# Calling PureScript From Python

You expect this is a large document? No, it's quite short:
1. `spago build`
2. `python -c "from <your project name>.Main.pure import main;main()"`

You can extract the generated Python package out, and encapsulate it as a standalone Python package, and upload to PyPI.

Module Path
------------------------------


Moreover, given such a PS module `A.B`:

```purescript
module A.B(export1, export2) where
...
```

In Python, you can import `export1` by

```python
import <your project name>.A.B.pure as AB
AB.export1 # you got it
```


Calling Convention
-------------------------------

This is also a reason why PureScript is pragmatic,
because it use the same calling convention like Python's.

It supports tail call optimizations, but, a purescript function like

```purescript
f x y = x + y
```

can be used in Python in this way:

```python
assert f(1)(2) == 3
```


Object Representation
-----------------------------------

- Record: `{a: 1, b: 2}`, in Python it's `{"a": 1, "b": 2}`.
- Newtype : For `newtype A = A Int`, `A 1` in Python is just `1`.
- Datatype(Algebraic data types): `data S = S1 Int Number | S2 Text`, `[S1 1 2.0, S2 "hello!"]` in Python is `[{"value0": 1, "value1": 2.0, ".t": <function S1>}, {"value0": "hello!", ".t": S2}].
`

```
```