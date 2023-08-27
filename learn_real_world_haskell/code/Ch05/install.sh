#!/usr/bin/env bash
# author:	Jacob Xie
# @date:	2023/08/26 22:32:28 Saturday
# @brief:

# in case of Distribution.Simple not found error in Setup.hs
# see https://stackoverflow.com/a/71027444/8163324
# rm -rf ~/.ghc

runghc Setup configure --prefix=$HOME --user
runghc Setup build
runghc Setup install
