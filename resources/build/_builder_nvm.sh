#!/usr/bin/env bash

# See also /docs/build/node.md

set -e
set -u

if [[ $# -lt 1 ]]; then
  echo "This script is called by shellHelperFunctions.sh, _select_node_version_with_nvm()"
  echo "during build.sh configure steps. It is not intended to be called directly, as it"
  echo "is a wrapper for nvm."
  exit 99
fi

REQUIRED_NODE_VERSION="$1"

if [[ -z "${NVM_DIR+x}" ]]; then
  export NVM_DIR="$HOME/.nvm"
fi

#
# nvm on macos and linux is a shell function. `source`ing it into our scripts is
# fragile because it uses some of the same variable names that we do. We have
# marked our most precious variables as `readonly`, which breaks nvm re-use of
# them also! Safest way to work around this is to load nvm in a child process
# and run it there. Downside is then of course we don't get the `PATH` changes
# that nvm does for us, so we have to do it ourselves back in the caller,
# `_select_node_version_with_nvm()`.
#
# Note: the readonly attribute for variables is not inherited by child
#       processes.
#
# See also https://github.com/nvm-sh/nvm/issues/3257 (among others)
#

type -t nvm >/dev/null || {
  source "$NVM_DIR/nvm.sh"
  type -t nvm >/dev/null || {
    echo "Failed to find nvm"
    exit 1
  }
}

nvm use "$REQUIRED_NODE_VERSION" || \
  (nvm install "$REQUIRED_NODE_VERSION" && nvm use "$REQUIRED_NODE_VERSION")

# Beware the hardcoded path below -- it should already be in the system PATH

mkdir -p "$HOME/.keyman"
rm -f "$HOME/.keyman/node"
ln -s "$NVM_BIN" "$HOME/.keyman/node"
