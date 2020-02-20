# PureScript Python

For implementation and code generation specification, check [Implementation](./Impl.md).

Currently we have a preview release `v0.1.0.0a`for trying out.

## Get Started

1. Installing `pspy-blueprint`

    - Install from [GitHub release page](https://github.com/purescript-python/purescript-python/releases).
    - Install from source(Need Haskell [stack](https://docs.haskellstack.org/en/stable/README)): clone this repo, and use command `stack install .`, which will install `pspy-blueprint` to your `.local` PATH.

2. \*Installing a CPython distribution.

    If you're already a user of CPython, you can skip this step.
    
    Otherwise, go to [this official download](https://www.python.org/downloads/) page,
    download and install any valid distribution(`>=3.5`).

3. Installing the Python package `purescripto`, which will provide you the command `pspy`.

   Possible commands for installing can be:

   ```bash
   # if `pip` is in PATH
   pip install -U purescripto
   # if `python` is in PATH
   python -m pip install -U purescripto
   # if `pip3` is the correct command to install pkgs to Python3
   pip3 install -U purescripto
   ```

4. Installing [nodejs](https://nodejs.org/en/), which is distributed with a command `npm`, and use `npm` to install `purescript` and its package manager `spago`:
   ```bash
   npm install -g purescript
   npm install -g spago
   ```
   You might check [PureScript: Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) for more details.

4. `git clone https://github.com/purescript-python/purescript-python-ffi-index ~/.pspy/mirrors/default`.

    **If you're using windows, remember to expand the user directory "~" to "C:\Users\twshe\<username>"**.

5. Create an empty folder called `hello-world` somewhere appropriate,get in, and call
   ```
   spago init  # init purescript project
   pspy --init # init purescript-python local configuration
   ```

6. Add a key `backend` with value `"pspy"`, to file `spago.dhall` of your `hello-world` project. This is an example:
  
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

7. Write your code in `src/**.purs`, and use `spago run` to execute your project(the default entry module is `Main`).


## PureScript Learning Materials

PureScript is close to Haskell, hence a Haskell user can pick it up in few seconds.

The home of PureScript is [PureScript.org](http://www.purescript.org/), where you can find things involving documentations.

## HOW-TO: IDE Support

A major motivation for my working on PureScript is its lightweighted but awesome IDE support.

For VSCode users, installing the plugin `PureScript IDE` and `File -> Preferences -> Settings -> (search purescript) -> Turn on "Add Spago sources"` will be sufficient. **No need to install some GitHub repo and build for 4 hours! And this IDE feels swift!**

## HOW-TO: Using Python FFI files for My PS Projects

For each project,
if your project directory tree is

```
- src
    - Main.purs
    - Mod
        - A.purs
```

If you have foreign definitions in module `Mod.A`, you need to
create a directory `python-ffi` juxtaposing `src`, and it'll finally look like:

```
- python-ffi
   - Mod
      - A.py
- src
    - Main.purs
    - Mod
        - A.purs
```
