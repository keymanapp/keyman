#!/usr/bin/env bash

set -eu

display_usage() {
  echo "Usage: $0 --build-path path"
  echo "       $0 --help"
  echo
  echo "This script is called from the Keyman Developer installer build"
  echo "and is not intended for standalone use."
  echo
  echo "  --help              displays this screen and exits"
  echo "  --build-path path   temporary path where archive will be built"
}

BUILD_PATH=

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -help|-h)
      display_usage
      exit
      ;;
    --build-path)
      shift
      BUILD_PATH="$1"
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit 64
  esac
  shift # past the processed argument
done

if [[ -z $BUILD_PATH ]]; then
  echo "Parameter --build-path is required"
  display_usage
  exit 64
fi

KEYMAN_WIX_TEMP_BASE="$BUILD_PATH"
KEYMAN_WIX_TEMP_MODELCOMPILER="$BUILD_PATH/ModelCompiler"
KEYMAN_MODELCOMPILER_ROOT="$KEYMAN_ROOT/developer/js"

# We use `npm pack` to extract only the aspects of the model-compiler actually needed for distribution.
# While we could use npm-bundle or similar, that adds extra, unwanted cruft; our approach gives us more
# control of the set of files distributed with the Keyman Developer installer.
#
# For users on other operating systems, node.js is a dependency and the compiler can be installed with
# `npm install @keymanapp/lexical-model-compiler`.

# We copy the files to a temp folder in order to exclude thumbs.db, .vs, etc from harvesting
rm -rf "$KEYMAN_WIX_TEMP_MODELCOMPILER"

# Step 1 - npm-pack the model compiler package, then unpack it in our bundling directory.
# This automatically strips the package to its barebones.
npm pack
mv keymanapp-lexical-model-compiler*.tgz kmlmc.tgz
mv kmlmc.tgz "$KEYMAN_WIX_TEMP_BASE"

# We extract the npm-packed version of the model compiler in order to reproduce our needed bundle.
cd "$KEYMAN_WIX_TEMP_BASE"
tar xvzf kmlmc.tgz

# Creates the directory referenced by $(KEYMAN_WIX_TEMP_MODELCOMPILER).
mv package ModelCompiler

# Cleans up the npm pack artifacts, which are no longer needed.
rm kmlmc.tgz

# Step 2 - the model compiler has one in-repo dependency that will also need to be packed.
# Managing other in-repo dependencies will be simpler; they install into ModelCompiler's
# extracted bundle.
cd "$KEYMAN_MODELCOMPILER_ROOT/node_modules/@keymanapp/models-types"
npm pack
mv keymanapp-models-types*.tgz kmtypes.tgz
mv kmtypes.tgz "$KEYMAN_WIX_TEMP_MODELCOMPILER"

# Step 3 - install just the bare essentials by using our packed local dependency, followed by
# all external production dependencies.
cd "$KEYMAN_WIX_TEMP_MODELCOMPILER"
# package-lock.json wasn't bundled; this is needed to keep dependency versions consistent.
cp "$KEYMAN_MODELCOMPILER_ROOT/package-lock.json" "$KEYMAN_WIX_TEMP_MODELCOMPILER/"

# as of npm v8.x, even though we are only working with `dependencies`, `devDependencies` is
# still checked, and as these two modules are present in devDependencies but are only
# available when in the repo path, we need to remove them before attempting to continue.
# Yuck! ref: https://github.com/npm/cli/issues/3975#issuecomment-985305678
# ref: https://github.com/npm/cli/issues/2921
# can't use npm uninstall because it depends on @keymanapp/models-types being present!

cat "$KEYMAN_MODELCOMPILER_ROOT/package.json" | grep -v "@keymanapp/models-templates" | grep -v "@keymanapp/models-wordbreakers" > package.json || true

npm install kmtypes.tgz --production --no-optional
npm install --production --no-optional

# Clean up the npm pack artifacts for the dependencies.
rm kmtypes.tgz

# We don't need the unit tests
rm -rf "$KEYMAN_WIX_TEMP_MODELCOMPILER/tests"
