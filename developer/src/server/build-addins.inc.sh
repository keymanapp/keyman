#!/usr/bin/env bash

# This script is used by build.sh

isFileX64() {
  [[ $(file -b "$1" | grep x86-64) ]] && echo 1 || echo 0
}

isNodeX64() {
  isFileX64 "$(which node)"
  # [[ $(file -b "$(which node)" | grep x86-64) ]] && echo 1 || echo 0
}

do_build_addins() {
  local NODEX64=$(isNodeX64)
  local ARCH=x64
  local TRAYICON_TARGET=addon.x64.node
  local HIDECONSOLE_TARGET=node-hide-console-window.x64.node
  echo "Building addins with $(which node)"

  if (( ! NODEX64 )); then
    ARCH=ia32
    TRAYICON_TARGET=addon.node
    HIDECONSOLE_TARGET=node-hide-console-window.node
  fi

  echo "NODEX64=$NODEX64"
  echo "ARCH=$ARCH"
  echo "TRAYICON_TARGET=$TRAYICON_TARGET"
  echo "HIDECONSOLE_TARGET=$HIDECONSOLE_TARGET"

  local HIDECONSOLE_SRC_TARGET="$KEYMAN_ROOT/developer/src/server/src/win32/console/$HIDECONSOLE_TARGET"
  local HIDECONSOLE_BIN_TARGET="$KEYMAN_ROOT/developer/src/server/build/src/win32/console/$HIDECONSOLE_TARGET"
  local TRAYICON_SRC_TARGET="$KEYMAN_ROOT/developer/src/server/src/win32/trayicon/$TRAYICON_TARGET"
  local TRAYICON_BIN_TARGET="$KEYMAN_ROOT/developer/src/server/build/src/win32/trayicon/$TRAYICON_TARGET"

  mkdir -p "$(dirname "$HIDECONSOLE_BIN_TARGET")"
  mkdir -p "$(dirname "$TRAYICON_BIN_TARGET")"

  #
  # Build node-windows-trayicon
  #

  pushd "$KEYMAN_ROOT/developer/src/server/src/win32/trayicon/addon-src"
  rm -rf build
  npm ci
  node-gyp clean configure build --arch=$ARCH --silent
  cp build/Release/addon.node "$TRAYICON_SRC_TARGET"
  cp build/Release/addon.node "$TRAYICON_BIN_TARGET"
  popd

  #
  # Build hetrodo-node-hide-console-window-napi
  #

  pushd "$KEYMAN_ROOT/node_modules/node-hide-console-window"
  rm -rf build
  node-gyp clean configure build --arch=$ARCH --silent
  cp build/Release/node-hide-console-window.node "$HIDECONSOLE_SRC_TARGET"
  cp build/Release/node-hide-console-window.node "$HIDECONSOLE_BIN_TARGET"
  popd

  #
  # Sanity check
  #

  if (( NODEX64 )); then
    [[ $(isFileX64 "$TRAYICON_BIN_TARGET") == 1 ]] || builder_die "$TRAYICON_TARGET should be 64-bit"
    [[ $(isFileX64 "$HIDECONSOLE_BIN_TARGET") == 1 ]] || builder_die "$HIDECONSOLE_TARGET should be 64-bit"
  else
    [[ $(isFileX64 "$TRAYICON_BIN_TARGET") == 0 ]] || builder_die "$TRAYICON_TARGET should not be 64-bit"
    [[ $(isFileX64 "$HIDECONSOLE_BIN_TARGET") == 0 ]] || builder_die "$HIDECONSOLE_TARGET should not be 64-bit"
  fi
}
