#!/usr/bin/env bash

# This script is included by each sample build.sh.  Accordingly, it inherits build-utils.sh, etc
# from the including script.

function do_build() {
  # Copy resources.
  cp -Rf "$KEYMAN_ENGINE_FRAMEWORK_SRC" "$KEYMAN_ENGINE_FRAMEWORK_DST"

  CODE_SIGN=
  if builder_is_debug_build; then
    CODE_SIGN=CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO CODE_SIGNING_ALLOWED="NO" CODE_SIGNING_ENTITLEMENTS=""
  fi

  run_xcodebuild -quiet \
                 $CODE_SIGN \
                 -target "$TARGET" \
                 -config "$CONFIG"
}

function execute_sample_build() {
  if [ -z "$TARGET" ]; then
    builder_die "Usage of `execute_sample_build` must specify a target iOS sample project"
  fi

  builder_describe "Builds sample app $TARGET that demos the Keyman Engine for iPhone and iPad" \
    "@/ios/engine build" \
    "clean" \
    "configure" \
    "build" \
    "--sim-artifact+  Unused by this build at present"

  builder_parse "$@"

  local CONFIG=Release
  if builder_is_debug_build; then
    CONFIG="Debug"
  fi

  local BUILD_FOLDER="ios/samples/$TARGET/build"

  builder_describe_outputs \
    build  "/$BUILD_FOLDER"

  ### START OF THE BUILD ###

  # NOT local b/c they're needed by do_build.
  KEYMAN_ENGINE_FRAMEWORK_SRC="$KEYMAN_ROOT/ios/build/Build/Products/$CONFIG/KeymanEngine.xcframework"
  KEYMAN_ENGINE_FRAMEWORK_DST=./

  builder_run_action clean     rm -rf "$KEYMAN_ROOT/$BUILD_FOLDER"
  builder_run_action build     do_build
}