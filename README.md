# Studies Haskell

## Haskell Installation

1. `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`

2. `cat ~/.ghcup/env`

3. [MacOS] copy the entire file into `~/.zshrc`

4. `ghcup tui` open a GUI to install the rest part, (`i` for install; `u` for uninstall; `s` for set and enable)

## Cabal

[User Guide](https://cabal.readthedocs.io/en/stable/index.html)

- Creating a package: <https://cabal.readthedocs.io/en/stable/cabal-package.html#creating-a-package>

- Package install: `cabal install --lib random`

### Lib Construction and Installation

1. `cabal init` to create a `.cabal` file

1. `touch Setup.hs` and put:

    ```hs
    #!/usr/bin/env runhaskell
    import Distribution.Simple
    main = defaultMain
    ```

1. `runghc Setup configure --prefix=$HOME --user`: config to user level's repo

1. `runghc Setup build`: build package

1. `runghc Setup install`: install package

1. `ghc-pkg list`: list all installed packages

1. `ghc-pkg unregister`: remove a package

## VsCode Setting

`settings.json` (use `ghcup tui` to get `ghc` & `hls` versions):

```json
  ...
  "haskell.manageHLS": "GHCup",
  "haskell.toolchain": {
    "ghc": "9.4.7",
    "hls": "2.4.0.0",
    "cabal": "recommended"
  },
  "haskell.serverEnvironment": {
    "PATH": "${HOME}/.ghcup/bin:$PATH"
  },
  ...
```

## Project Management

See [implicit-hie](https://github.com/Avi-D-coder/implicit-hie)

```sh
cd your-stack-or-cabal-package
stack install implicit-hie # or cabal install implicit-hie
gen-hie > hie.yaml
```

## Test

- compile a single file: `ghc --make capslocker.hs`

## Misc

- While using GHCI, it might show `libgmp.so: cannot open shared object file: No such file or directory`, and to solve this we can `apt install libgmp-dev`.

- `Cabal-fmt` installation: `cabal install cabal-fmt`

## Cheatsheets

- [Symbol](./cheatsheets/symbol.tex)

## Study Material

- [Learn Me a Haskell](./Learn%20Me%20a%20Haskell.pdf): reading notes from "Learn you a haskell" [ [link](http://learnyouahaskell.com/) ]

  - ~~Ch01 Introduction~~

  - [Ch02 Starting Out](./learn_me_a_haskell/Ch02%20Starting%20Out.tex)

  - [Ch03 Types and Typeclasses](./learn_me_a_haskell/Ch03%20Types%20and%20Typeclasses.tex)

  - [Ch04 Syntax in Functions](./learn_me_a_haskell/Ch04%20Syntax%20in%20Functions.tex)

  - [Ch05 Recursion](./learn_me_a_haskell/Ch05%20Recursion.tex)

  - [Ch06 Higher Order Functions](./learn_me_a_haskell/Ch06%20Higher%20Order%20Functions.tex)

  - [Ch07 Modules](./learn_me_a_haskell/Ch07%20Modules.tex)

  - [Ch08 Making Our Own Types and Typeclasses](./learn_me_a_haskell/Ch08%20Making%20Our%20Own%20Types%20and%20Typeclasses.tex)

  - [Ch09 Input and Output](./learn_me_a_haskell/Ch09%20Input%20and%20Output.tex)

  - [Ch10 Functionally Solving Problems](./learn_me_a_haskell/Ch10%20Functionally%20Solving%20Problems.tex)

  - [Ch11 Functors, Applicative Functors and Monoids](./learn_me_a_haskell/Ch11%20Functors,%20Applicative%20Functors%20and%20Monoids.tex)

  - [Ch12 A Fistful of Monads](./learn_me_a_haskell/Ch12%20A%20Fistful%20of%20Monads.tex)

  - [Ch13 For a Few Monads More](./learn_me_a_haskell/Ch13%20For%20a%20Few%20Monads%20More.tex)

  - [Ch14 Zippers](./learn_me_a_haskell/Ch14%20Zippers.tex)

- [Learn Real World Haskell](./Learn%20Real%20World%20Haskell.pdf): reading notes from "Real World Haskell" [ [link](https://book.realworldhaskell.org/read/) ]

  - ~~Ch1 Getting started~~

  - ~~Ch2 Types and functions~~

  - ~~Ch3 Defining types, streamlining functions~~

  - ~~Ch4 Functional programming~~

  - [Ch5 Writing a library: working with JSON data](./learn_real_world_haskell/Ch5%20Writing%20a%20library:%20working%20with%20JSON%20data.tex)

  - [Ch6 Using typeclasses](./learn_real_world_haskell/Ch6%20Using%20typeclasses.tex)

  - [Ch7 Input and output](./learn_real_world_haskell/Ch7%20Input%20and%20output.tex)

  - [Ch8 Efficient file processing, regular expressions, and file name matching](./learn_real_world_haskell/Ch8%20Efficient%20file%20processing,%20regular%20expressions,%20and%20file%20name%20matching.tex)

  - [Ch9 I/O case study: a library for searching the filesystem](./learn_real_world_haskell/Ch9%20IO%20case%20study:%20a%20library%20for%20searching%20the%20filesystem.tex)

  - Ch10 Code case study: parsing a binary data format

  - Ch11 Testing and quality assurance

  - Ch12 Barcode recognition

  - Ch13 Data structures

  - Ch14 Monads

  - Ch15 Programming with monads

  - Ch16 The Parsec parsing library

  - Ch17 The foreign function interface

  - Ch18 Monad transformers

  - Ch19 Error handling

  - Ch20 Systems programming

  - Ch21 Working with databases

  - Ch22 Web client programming

  - Ch23 GUI programming

  - Ch24 Basic concurrent and parallel programming

  - Ch25 Profiling and tuning for performance

  - Ch26 Advanced library design: building a Bloom filter

  - Ch27 Network programming

  - Ch28 Software transactional memory

## Reference

- [How to install Haskell toolchains (CN)](https://zhuanlan.zhihu.com/p/455688955)
