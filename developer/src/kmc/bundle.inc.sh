#!/usr/bin/env bash

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

bundle() {
  if [[ $# -eq 0 ]]; then
    echo "Parameter --build-path is required"
    exit 64
  fi

  local BUILD_PATH="$1"

  # Input may be from a Windows build script, so we'll use cygpath to translate
  case $(uname) in
    CYGWIN* )
      BUILD_PATH=$(cygpath -u "$BUILD_PATH")
      ;;
    MINGW* )
      BUILD_PATH=$(cygpath -u "$BUILD_PATH")
      ;;
  esac

  KEYMAN_WIX_TEMP_BASE="$BUILD_PATH"
  KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER="$BUILD_PATH/LDMLKeyboardCompiler"
  KEYMAN_LDMLKEYBOARDCOMPILER_TEMP=`mktemp -d`

  echo "Temporary build path is $KEYMAN_LDMLKEYBOARDCOMPILER_TEMP"

  mkdir -p "$KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER"

  # We copy the built compiler into a temporary folder in order to prepare
  # the node modules, as otherwise the workspace will interfere

  cp -R "$KEYMAN_ROOT/developer/src/kmc/"* "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP/"

  cd "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP"

  # We use `npm pack` to extract only the aspects of the compiler actually
  # needed for distribution. While we could use npm-bundle or similar, that adds
  # extra, unwanted cruft; our approach gives us more control of the set of
  # files distributed with the Keyman Developer installer.
  #
  # For users on other operating systems, node.js is a dependency and the
  # compiler can be installed with `npm install
  # @keymanapp/ldml-keyboard-compiler`.

  # We copy the files to a temp folder in order to exclude thumbs.db, .vs, etc
  # from harvesting
  rm -rf "$KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER"

  # Step 1 - npm-pack the compiler package, then unpack it in our bundling
  # directory. This automatically strips the package to its barebones.
  set_npm_version
  npm pack
  mv keymanapp-ldml-keyboard-compiler*.tgz kmc.tgz
  mv kmc.tgz "$KEYMAN_WIX_TEMP_BASE"

  # We extract the npm-packed version of the compiler in order to reproduce
  # our needed bundle.
  cd "$KEYMAN_WIX_TEMP_BASE"
  tar xvzf kmc.tgz

  # Creates the directory referenced by $(KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER).
  mv package LDMLKeyboardCompiler

  # Cleans up the npm pack artifacts, which are no longer needed.
  rm kmc.tgz

  # Step 2 - the compiler has one in-repo dependency that will also need to
  # be packed. Managing other in-repo dependencies will be simpler; they install
  # into LDMLKeyboardCompiler's extracted bundle.

  mkdir -p "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP/node_modules/@keymanapp/"
  cp -R "$KEYMAN_ROOT/node_modules/@keymanapp/keyman-version" "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP/node_modules/@keymanapp/"

  cd "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP/node_modules/@keymanapp/keyman-version"
  set_npm_version
  npm pack
  mv keymanapp-keyman-version*.tgz kmver.tgz
  mv kmver.tgz "$KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER"

  # Step 3 - install just the bare essentials by using our packed local
  # dependency, followed by all external production dependencies.
  cd "$KEYMAN_WIX_TEMP_LDMLKEYBOARDCOMPILER"
  # package-lock.json wasn't bundled; this is needed to keep dependency versions
  # consistent.

  # as of npm v8.x, even though we are only working with `dependencies`,
  # `devDependencies` is still checked, and as these modules are present in
  # devDependencies but are only available when in the repo path, we need to
  # remove them before attempting to continue.
  # Yuck! ref: https://github.com/npm/cli/issues/3975#issuecomment-985305678
  # ref: https://github.com/npm/cli/issues/2921

  cat "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP/package.json" | "$JQ" \
    '. | del(.devDependencies."@keymanapp/keyman-version")' \
    > "package.json"

  npm install kmver.tgz --production --no-optional
  npm install --production --no-optional

  # Clean up the npm pack artifacts for the dependencies.
  rm -rf kmver.tgz

  # We don't need the unit tests, source files, or build scripts
  rm -rf test src *.sh Makefile tsconfig*

  # Clean up our temporary build folder
  rm -rf "$KEYMAN_LDMLKEYBOARDCOMPILER_TEMP"
}