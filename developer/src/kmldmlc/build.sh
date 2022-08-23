#!/usr/bin/env bash
#
# Compiles the developer tools, including the language model compilers.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman LDML Keyboard Compiler kmldmlc" \
  "configure     runs 'npm ci' on root folder" \
  "build         (default) builds kmldmlc to build/" \
  "clean         cleans build/ folder" \
  "bundle        creates a bundled version of kmldmlc" \
  "test          run automated tests for kmldmlc" \
  "publish       publish to npm" \
  "--build-path=BUILD_PATH  Build directory for bundle" \
  "--dry-run,-n  Don't actually publish, just dry run"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action configure; then
  verify_npm_setup
  builder_report success configure
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_report success clean
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action build; then
  npm run build
  builder_report success build
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action test; then
  npm test
  builder_report success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action bundle; then
  . ./bundle.inc.sh
  bundle "$BUILD_PATH"
  builder_report success bundle
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action publish; then
  if [[ $TIER == stable ]]; then
    npm_dist_tag=latest
  else
    npm_dist_tag=$TIER
  fi

  set_npm_version

  if build_has_option --dry-run; then
    DRY_RUN=--dry-run
  else
    DRY_RUN=
  fi

  #TEMP: we don't want to publish at this point!
  DRY_RUN=--dry-run

  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  echo "Publishing $DRY_RUN npm package with tag $npm_dist_tag"
  npm publish $DRY_RUN --access public --tag $npm_dist_tag || fail "Could not publish $npm_dist_tag release."

  builder_report success publish
fi
