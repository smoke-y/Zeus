#!/bin/bash

mkdir -p bin/lin

#clang++ src/main.cc -O2 -march=native -o bin/lin/zeus.o -D LIN=1 -D SIMD=1
clang++ -g -O0 src/main.cc -o bin/lin/zeus_dbg -D LIN=1 -D SIMD=1 -D DBG=1

bin/lin/zeus_dbg test/t6.zs bin/lin/out.ir
