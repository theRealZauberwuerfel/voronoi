#!/bin/sh

ghc --make -O2 -fllvm -fast-llvm -fforce-recomp -threaded Main.hs -o main
