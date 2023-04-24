#!/usr/bin/env bash

set -eu

echo "--- Full clean configure build install test build ---"
./build.sh clean configure build install test
if [ $(ls child?.* | wc -l) -ne 14 ]; then
  echo unexpected output file count
  exit 1
fi

echo "--- Test error result for child build ---"
./build.sh error && (echo should not have passed; exit 1) || (echo "  ... build returned error as expected")

echo "--- Test building only specific actions and targets (install:child1 test:child2) ---"
./build.sh install:child1 test:child2
if [ $(ls child?.* | wc -l) -ne 2 ]; then
  echo unexpected output file count
  exit 1
fi

echo "--- Test that --no-deps is passed correctly to child scripts ---"
./build.sh --no-deps
if [ -f ./dep.configure ] || [ -f ./dep.build ]; then
  echo unexpected dep.configure or dep.build file, should not have built this because of --no-deps flag
  exit 1
fi