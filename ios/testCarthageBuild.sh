#!/usr/bin/env bash

cd ..
rm -rf Carthage
carthage checkout --use-submodules
carthage build --no-skip-current --use-xcframeworks --platform ios --no-use-binaries

