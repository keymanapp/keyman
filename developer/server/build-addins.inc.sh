#!/usr/bin/env bash

# This script is used by build.sh

isNodeX64() {
  [[ $(file -b "$(which node)" | grep x86-64) ]] && echo 1 || echo 0
}

build_addins() {
  local NODEX64=$(isNodeX64)
  local ARCH=x64
  local TRAYICON_TARGET=addon.x64.node
  local HIDECONSOLE_TARGET=node-hide-console-window.x64.node
  echo "Building addins with $(which node)"

  if (( NODEX64 )); then
    ARCH=ia32
    TRAYICON_TARGET=addon.node
    HIDECONSOLE_TARGET=node-hide-console-window.node
  fi

  echo "NODEX64=$NODEX64"
  echo "ARCH=$ARCH"
  echo "TRAYICON_TARGET=$TRAYICON_TARGET"
  echo "HIDECONSOLE_TARGET=$HIDECONSOLE_TARGET"

  #
  # Build node-windows-trayicon
  #

  pushd "$KEYMAN_ROOT/developer/server/win32/node-windows-trayicon"
  rm -rf node_modules
  rm -rf build
  npm install --arch=$ARCH
  cp build/Release/addon.node "$KEYMAN_ROOT/developer/server/src/win32/trayicon/$TRAYICON_TARGET"
  popd

  #
  # Build hetrodo-node-hide-console-window-napi
  #

  pushd "$KEYMAN_ROOT/developer/server/win32/hetrodo-node-hide-console-window-napi"
  rm -rf node_modules
  rm -rf build
  npm install --arch=$ARCH
  cp build/Release/node-hide-console-window.node "$KEYMAN_ROOT/developer/server/src/win32/console/$HIDECONSOLE_TARGET"
  popd
}
