#!/usr/bin/env bash

# The diagram is built using the Graphviz suite.
# You can get it with most package managers, e.g.,
#
#   sudo apt installl graphviz  # Ubuntu
#
#   brew install graphviz  # macOS

# Check if Graphviz/dot is installed
if ! hash dot ; then
  echo "Cannot (re)build state diagram" 1>&2
  echo "Missing the Graphviz suite" 1>&2
  echo "Download at $(tput bold)https://www.graphviz.org/$(tput sgr0)"
  exit 1
fi

dot -Tpng lmlayer-states.dot -o lmlayer-states.png
