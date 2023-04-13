#!/usr/bin/env bash

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" != "darwin"* ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

IMLIBRARY_PATH="$HOME/Library/Input Methods"

display_usage() {
    echo "Use localdeploy.sh to deploy Keyman Input Method app to $IMLIBRARY_PATH."
    echo "This kills the existing (running) process if needed. Typically called from"
    echo "the Keyman mac build script."
    echo
    echo "usage: localdeploy.sh keymanInputMethodAppPath"
    echo
    echo "where keymanInputMethodAppPath is the path to a folder which contains the"
    echo "(codesigned) Keyman Input Method app to be deployed locally."
    exit 1
}

if [[ $# != 1 ]]; then
    display_usage
fi

KMIM_APPNAME="Keyman.app"
KM4MIM_APP_PATH="$1/$KMIM_APPNAME"

if ! [[ -d "$KM4MIM_APP_PATH" ]]; then
    display_usage
fi

pid=$(pgrep Keyman)
if [[ -n $pid ]]; then
	kill $pid
fi

KM4MIM_INSTALL_PATH="$IMLIBRARY_PATH/$KMIM_APPNAME"

if [[ -d "$KM4MIM_INSTALL_PATH" ]]; then
    rm -Rf "$KM4MIM_INSTALL_PATH"
fi
echo "Copying $KM4MIM_APP_PATH to $IMLIBRARY_PATH..."
/bin/cp -R "$KM4MIM_APP_PATH" "$IMLIBRARY_PATH"

if [[ -d "$KM4MIM_INSTALL_PATH" ]]; then
    exit 0
fi

exit 1