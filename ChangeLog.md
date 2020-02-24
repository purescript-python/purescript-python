# Changelog for purescript-python

2020-02-21: Version 0.1.0.0a(Preview)
-------------------------------------------------

- A preview release has finally come.


2020-02-22: Version 0.1.0.0
-------------------------------------------------

- Statically linked Linux executable under License LGPL


2020-02-24: Version 0.1.1.0
--------------------------------------


- Fix multi-param datatype constructors
- Fix throwing exceptions
- Fix the use of some JS specific features in CodeGen:

  PS codegen uses JS tricks like `"a" + ["b", "c"] == "ab,c"`,
  we change the generated code like `throw new Error("a" + ["b", "c"])`
  to `throw Error("a" + ",".join(["b", "c"]))`.
  
  \*Further optimizations can be done later.
