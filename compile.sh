#!/bin/bash

JS=.stack-work/install/x86_64-linux/nightly-2018-05-10/ghcjs-8.4.0.1_ghc-8.4.2.20180505/bin/ghcjs-form-sample1-exe.jsexe/all.js
TARGET=html/compiled.js
COMPILER=lib/compiler-latest/closure-compiler-v20190528.jar

java -jar $COMPILER \
     --compilation_level=ADVANCED_OPTIMIZATIONS \
     --define 'goog.DEBUG=false' \
     --jscomp_off=checkVars \
     $JS \
     > $TARGET
