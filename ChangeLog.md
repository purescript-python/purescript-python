# Changelog for purescript-python

2020-03-05: Version 0.1.2.0
---------------------------------

- codegen support for JavaScript-specific behaviors: add default value `None` for each function argument. fix #10
- faster codegen: avoid pretty print when not specified. #13
- lighter volume: for generated files. type level computing can produce huge amount of code, to make generated code Git maintainable, we use bzip2 to compress stuffs. #13
- options for pspy-blueprint changed(`--py-dir <dir0> --entry-mod <A.B.C> --ffi-dep <xxx> --out-format [Pretty|Compact|Compressed]`).
- special optimization for repeating indexing and attribute accessing.
- the `topdown` file format to allow Python to load large scale code, and load code faster. fix #13
- tagless final approach to support multiple data formats of output IR for codegen,
  check `Topdown/{Raw.hs, Topdown.hs, Pretty.hs}.`.
- add CI tests based on a purescript-python project. #15



2020-02-24: Version 0.1.1.0
--------------------------------------


- Fix multi-param datatype constructors
- Fix throwing exceptions
- Fix the use of some JS specific features in CodeGen:

  PS codegen uses JS tricks like `"a" + ["b", "c"] == "ab,c"`,
  we change the generated code like `throw new Error("a" + ["b", "c"])`
  to `throw Error("a" + ",".join(["b", "c"]))`.
  
  \*Further optimizations can be done later.



2020-02-22: Version 0.1.0.0
-------------------------------------------------

- Statically linked Linux executable under License LGPL

2020-02-21: Version 0.1.0.0a(Preview)
-------------------------------------------------

- A preview release has finally come.
