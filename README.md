# Studies Haskell

## Haskell Installation

1. `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`

2. `cat ~/.ghcup/env`

3. [MacOS] copy the entire file into `~/.zshrc`

4. `ghcup tui` open a GUI to install the rest part

5. `ghcup set stack` to enable `stack` command

## Cabal

[User Guide](https://cabal.readthedocs.io/en/stable/index.html)

- Creating a package: <https://cabal.readthedocs.io/en/stable/cabal-package.html#creating-a-package>

## VsCode Setting

`settings.json` (use `ghcup tui` to get versions):

```json
  ...
  "haskell.manageHLS": "GHCup",
  "haskell.toolchain": {
    "ghc": "9.2.8",
    "hls": "2.0.0.1",
    "cabal": "recommended"
  },
  "haskell.serverEnvironment": {
    "PATH": "${HOME}/.ghcup/bin:$PATH"
  },
  ...
```

## Misc

- While using GHCI, it might show `libgmp.so: cannot open shared object file: No such file or directory`, and to solve this we can `apt install libgmp-dev`.

- `Cabal-fmt` installation: `cabal install cabal-fmt`

## Study Material

- [Learn me a haskell](./Learn%20Me%20a%20Haskell.pdf): reading notes from "Learn you a haskell" [ [link](http://learnyouahaskell.com/) ]

## Reference

- [How to install Haskell toolchains (CN)](https://zhuanlan.zhihu.com/p/455688955)
