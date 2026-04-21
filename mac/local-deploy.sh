#!/usr/bin/env bash

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" != "darwin"* ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

IMLIBRARY_PATH="$HOME/Library/Input Methods"
APPS_PATH="/Applications"

display_usage() {
    echo "Use local-deploy.sh to deploy the Keyman Input Method app to $IMLIBRARY_PATH"
    echo "and the Keyman Configuration app to $APPS_PATH"
    echo "This kills the existing (running) process if needed. Typically called from"
    echo "the Keyman mac build script."
    echo
    echo "usage: local-deploy.sh keymanInputMethodPath keymanConfigAppPath"
    echo
    echo "where keymanInputMethodPath is the path to a folder containing the"
    echo "Keyman Input Method app and keymanConfigAppPath is the path to the"
    echo "folder containing the Keyman Configuration app."
    exit 1
}

if [ "$#" -ne 2 ]; then
    display_usage
fi

KMIM_APPNAME="Keyman.app"
KM4MIM_APP_PATH="$1/$KMIM_APPNAME"

# verify that input method exists at path
if ! [[ -d "$KM4MIM_APP_PATH" ]]; then
    echo "input method does not exist at path $KM4MIM_APP_PATH"
    display_usage
fi

KM_CONFIG_APPNAME="Keyman Configuration.app"
KM_CONFIG_APP_PATH="$2/$KM_CONFIG_APPNAME"

# verify that the config app exists at path
if ! [[ -d "$KM_CONFIG_APP_PATH" ]]; then
    echo "config app does not exist at path $KM_CONFIG_APP_PATH"
    display_usage
fi

# kill both apps
pid=$(pgrep Keyman)
if [[ -n $pid ]]; then
	kill $pid
fi

pid=$(pgrep 'Keyman Configuration')
if [[ -n $pid ]]; then
	kill $pid
fi

# remove existing input method and install new one
KM4MIM_INSTALL_PATH="$IMLIBRARY_PATH/$KMIM_APPNAME"

if [[ -d "$KM4MIM_INSTALL_PATH" ]]; then
    rm -Rf "$KM4MIM_INSTALL_PATH"
fi
echo "Copying $KM4MIM_APP_PATH to $IMLIBRARY_PATH..."
/bin/cp -R "$KM4MIM_APP_PATH" "$IMLIBRARY_PATH"

# remove existing configuration app and install new one
KMCONFIG_INSTALL_PATH="$APPS_PATH/$KM_CONFIG_APPNAME"

if [[ -d "$KMCONFIG_INSTALL_PATH" ]]; then
    rm -Rf "$KMCONFIG_INSTALL_PATH"
fi
echo "Copying $KM_CONFIG_APP_PATH to $APPS_PATH..."
/bin/cp -R "$KM_CONFIG_APP_PATH" "$APPS_PATH"

# exit successfully if Keyman is installed
if [[ -d "$KM4MIM_INSTALL_PATH" ]]; then
    exit 0
fi

exit 1