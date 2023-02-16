#!/usr/bin/env bash

set -eu

./build.sh clean configure build install test
if [ $(ls child?.* | wc -l) -ne 14 ]; then
  echo unexpected output file count
  exit 1
fi

./build.sh error && (echo should not have passed; exit 1) || (echo "  ... build returned error as expected")

./build.sh install:child1 test:child2
if [ $(ls child?.* | wc -l) -ne 2 ]; then
  echo unexpected output file count
  exit 1
fi