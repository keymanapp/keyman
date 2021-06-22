#!/usr/bin/env bash

set -e
set -u

## Note: does not use standard build script include
# adjust relative paths as necessary. Note that this will not work in a symlinked path reliably
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR=$(dirname "$THIS_SCRIPT")

# Determine which components to install
REQUIRE_ANDROID=false
REQUIRE_IOS=false
REQUIRE_MACOS=false
REQUIRE_WEB=false

# Optional components
REQUIRE_KMCOMP=false
REQUIRE_PANDOC=false
REQUIRE_SENTRYCLI=false

# Parse args
shopt -s nocasematch

PARAMFOUND=false

function print_help() {
    echo "Usage: macos.sh targets"
    echo "  targets: one or more of: "
    echo "             android ios macos web all"
    echo "  optional targets:"
    echo "             kmcomp pandoc sentry-cli"
    echo "Targets will automatically include dependency targets."
}

while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        --help)
            print_help
            exit 0
            ;;
        android)
            REQUIRE_ANDROID=true
            PARAMFOUND=true
            ;;
        ios)
            REQUIRE_IOS=true
            PARAMFOUND=true
            ;;
        macos)
            REQUIRE_MACOS=true
            PARAMFOUND=true
            ;;
        web)
            REQUIRE_WEB=true
            PARAMFOUND=true
            ;;
        kmcomp)
            REQUIRE_KMCOMP=true
            PARAMFOUND=true
            ;;
        pandoc)
            REQUIRE_PANDOC=true
            PARAMFOUND=true
            ;;
        sentry-cli)
            REQUIRE_SENTRYCLI=true
            PARAMFOUND=true
            ;;
        all)
            REQUIRE_ANDROID=true
            REQUIRE_IOS=true
            REQUIRE_MACOS=true
            REQUIRE_WEB=true
            PARAMFOUND=true
            ;;
        *)
            echo "Error: unrecognised parameter."
            echo
            print_help
            exit 1
            ;;
    esac
    shift
done

if ! $PARAMFOUND; then
    echo "Error: must have a target parameter"
    echo
    print_help
    exit 1
fi

# This script will configure your development environment from a bare metal mac install. It should be idempotent.

echo "This script will configure your macOS computer to build Keyman, installing build tools and prerequisites."
echo "You can also do this yourself following the notes in building.md."
echo
read -p "Press ENTER to start install"

if $REQUIRE_IOS || $REQUIRE_ANDROID; then
  REQUIRE_WEB=true
fi

if $REQUIRE_ANDROID; then
  REQUIRE_PANDOC=true
fi

## These components are required by all development toolchains

# XCode command lines tools will be installed by Brew
# xcode-select --install

which brew || (
    # Install Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
) && (
    brew update
)

## Install devchain components

BREW_ALL="bash jq python3 meson ninja rustup-init coreutils"
BREW_WEB="node emscripten wasm-pack openjdk@8"
BREW_IOS="swiftlint carthage"
BREW_MACOS="carthage cocoapods"
BREW_ANDROID="openjdk@8 android-sdk android-studio ant gradle maven"

brew install $BREW_ALL
$REQUIRE_ANDROID && brew install $BREW_ANDROID
$REQUIRE_IOS && brew install $BREW_IOS
$REQUIRE_WEB && brew install $BREW_WEB
$REQUIRE_MACOS && brew install $BREW_MACOS

$REQUIRE_PANDOC && brew install pandoc
$REQUIRE_SENTRYCLI && brew install getsentry/tools/sentry-cli

# Install WINE
$REQUIRE_KMCOMP && (
  brew tap homebrew/cask-versions
  brew install --cask --no-quarantine wine-stable
)

$REQUIRE_WEB && (
    rustup-init -y -t wasm32-unknown-unknown
)

source "$THIS_DIR/keyman.macos.env.sh"

$REQUIRE_ANDROID && (
    mkdir -p .android && touch ~/.android/repositories.cfg

    which sdkmanager || (
        echo "Please run Android Studio to install the Android SDK tools."
        read -p "Press ENTER to continue after installation"
        PATH="$HOME/Library/Android/sdk/tools/bin:$PATH"
    )
    sdkmanager "system-images;android-30;google_apis_playstore;x86_64"
    sdkmanager --update
    sdkmanager --licenses
)

# For now, we won't run this step automatically
# as it may interfere with other dev environments
#($REQUIRE_IOS || $REQUIRE_MACOS) && (
#    # Assumes that xcode is installed into normal path
#    # as otherwise we get only the command line tools which
#    # won't build with xcode
#    echo "Selecting default xcodebuild command line tools from /Applications/Xcode.app"
#    sudo xcode-select -s /Applications/Xcode.app
#)

# Add keyman.macos.env.sh to ~/.bashrc

echo "Adding environment variables to ~/.bashrc..."
if [ ! -f ~/.bashrc ] || `grep "keyman.macos.env.sh" ~/.bashrc 2>/dev/null`; then
    echo "source $THIS_DIR/keyman.macos.env.sh" >> ~/.bashrc
fi

echo "Configuration has completed successfully."
echo

if $REQUIRE_MACOS || $REQUIRE_IOS; then
    echo "The following components must be installed manually:"
    echo " * XCode"
    echo
fi

if $REQUIRE_MACOS || $REQUIRE_IOS || $REQUIRE_ANDROID; then
    echo "The following components should be started manually after this script completes, in order to install"
    echo "additional components:"
    if $REQUIRE_MACOS || $REQUIRE_IOS; then
        echo " * XCode"
    fi
    if $REQUIRE_ANDROID; then
        echo " * Android Studio"
    fi
    echo
fi
