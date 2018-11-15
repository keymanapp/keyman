#!/bin/sh

# Steps for doing iterative TDD:
# Usage:
#
#   ag -l | entr -c sh tdd.sh
npm run tsc -- -p ./worker/tsconfig.json &&\
  npm run tsc &&\
  npm run mocha --recursive ./unit_tests/headless/*.js
exit $?
