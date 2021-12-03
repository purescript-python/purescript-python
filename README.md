
# PureScript to Python Compiler

[![Build Status](https://travis-ci.com/thautwarm/purescript-python.svg?branch=master)](https://travis-ci.com/thautwarm/purescript-python) [![gitter room](https://img.shields.io/badge/chat-tagful&nbsp;initial-Pink.svg?style=flat)](https://gitter.im/reliable-python/community) [![install](https://img.shields.io/badge/install-oneliner-blue.svg?style=flat)](https://github.com/purescript-python/installer/) [![py interop](https://img.shields.io/badge/interop-purescript↔python-teal.svg?style=flat)](./Interops.md)

- [Specification and Implementation](./Impl.md)
- [Example Project](https://github.com/purescript-python/example-hw)

## Get Started

0. \*Install a CPython distribution.

    If you're already a user of CPython, you can skip this step.
    
    Otherwise, go to [this official download](https://www.python.org/downloads/) page,
    download and install any valid distribution(`>=3.5`, **<=3.8**).


1. Install [nodejs](https://nodejs.org/en/), which is distributed with a command `npm`, and use `npm` to install `purescript` and its package manager `spago`. The latest version of the purescript compiler that is supported is **0.13.8**:
   ```bash
   npm install -g purescript
   npm install -g spago
   ```
   You might check [PureScript: Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) for more details.

2. Install PureScript-Python components:

   `curl -fsSL https://raw.githubusercontent.com/purescript-python/installer/master/install.sh | bash`

3. Create an empty folder called `hello-world` somewhere appropriate,get in, and call
   ```
   spago init  # init purescript project
   pspy --init # init purescript-python local configuration
   ```

4. Add a key `backend` with value `"pspy"`, to file `spago.dhall` of your `hello-world` project. This is an example:
  
   ```dhall
    {-
    Welcome to a Spago project!
    You can edit this file as you like.
    -}
    { name = "my-project"
    , dependencies = [ "console", "effect", "psci-support" ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    , backend = "pspy" -- !!NOTE THIS!!
    }
   ```

5. Write your code in `src/**.purs`, and use `spago run` to execute your project(the default entry module is `Main`).


## PureScript Learning Materials

PureScript is close to Haskell, hence a Haskell user can pick it up in few seconds.

The home of PureScript is [PureScript.org](http://www.purescript.org/), where you can find things involving documentations.


## HOW-TO: IDE Support

A major motivation for my working on PureScript is its lightweighted but awesome IDE support.

For VSCode users, installing the plugin `PureScript IDE` and `File -> Preferences -> Settings -> (search purescript) -> Turn on "Add Spago sources"` will be sufficient. **No need to install some GitHub repo and build for 4 hours! And this IDE feels swift!**

## Troubleshoot `pspy-blueprint`

If `pspy-blueprint` provided by the Python package `purescripto` didn't work(e.g., users of MacOSX < 10.15), you should manually install it from this repository, and currently there're 2 options:

1. Install from [GitHub release page](https://github.com/purescript-python/purescript-python/releases).
2. Install from source(Need Haskell [stack](https://docs.haskellstack.org/en/stable/README)): clone this repo, and use command `stack install .`, which will install `pspy-blueprint` to your `.local` PATH.

For Linux users, you might also need to use `chmod u+x <path/to/pspy-blueprint>` to allow the permission to execute.

## Troubleshoot: Execution Not Sync to Latest Code

This seems to be a recent issue produced by the upstream compiler, and you can resolve this by removing the current `output` directory:

```bash
rm -rf $YOUR_PROJECT_ROOT/output && spago build && pspy --run
```

This will produce the result of your latest code.

