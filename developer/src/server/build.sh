#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

builder_describe "Build Keyman Developer Server" \
  @/common/web/keyman-version \
  @/developer/src/common/web/utils \
  @/web \
  clean \
  configure \
  build \
  test \
  "installer   Prepare for Keyman Developer installer" \
  "publish     Publish to NPM" \
  ":server     Keyman Developer Server main program" \
  ":addins     Windows addins for GUI integration"

builder_describe_internal_dependency \
  publish:server       build:server \
  publish:server       build:addins

builder_describe_outputs \
  configure:server     /node_modules \
  configure:addins     /node_modules \
  build:server         /developer/src/server/build/src/index.js \
  build:addins         /developer/src/server/build/src/win32/trayicon/addon.node

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function clean_server() {
  rm -rf "$THIS_SCRIPT_PATH/build"
  rm -rf "$THIS_SCRIPT_PATH/node_modules"
  rm -rf "$THIS_SCRIPT_PATH/tsconfig.tsbuildinfo"
  rm -rf "$THIS_SCRIPT_PATH/src/site/resource"

  # No longer in use, cleanup from previous versions:
  rm -rf "$THIS_SCRIPT_PATH/dist"
}

function configure_server() {
  verify_npm_setup
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  # TODO: this is horrible; is there a way we can avoid this?
  rm -f "$KEYMAN_ROOT"/node_modules/ngrok/bin/ngrok.exe
}

function build_addins() {
  # Rebuild and bundle addins
  source ./build-addins.inc.sh

  # If we have an x64 version of node installed
  if [[ $(isNodeX64) ]]; then
    do_build_addins
  fi

  # Build with the Keyman Developer x86 version of node
  PATH="$KEYMAN_ROOT/developer/src/inst/node/dist:$PATH" do_build_addins
}

function build_server() {
  # Copy keymanweb build artifacts
  local WEB_SRC="$KEYMAN_ROOT/web/build/publish/debug"
  local DST="$THIS_SCRIPT_PATH/src/site/resource"

  rm -rf "$DST"
  mkdir -p "$DST/osk"
  mkdir -p "$DST/ui"
  cp "$WEB_SRC/"*.js "$WEB_SRC/"*.js.map "$DST/"
  cp -R "$WEB_SRC/osk/"* "$DST/osk/"
  cp -R "$WEB_SRC/ui/"* "$DST/ui/"
  cp "$KEYMAN_ROOT/web/LICENSE" "$DST/"
  cp "$KEYMAN_ROOT/web/README.md" "$DST/"

  # Build server
  tsc --build

  # Post build
  mkdir -p "$THIS_SCRIPT_PATH/build/src/site/"
  mkdir -p "$THIS_SCRIPT_PATH/build/src/win32/"
  cp -r "$THIS_SCRIPT_PATH/src/site/"** "$THIS_SCRIPT_PATH/build/src/site/"
  cp -r "$THIS_SCRIPT_PATH/src/win32/"** "$THIS_SCRIPT_PATH/build/src/win32/"

  replaceVersionStrings "$THIS_SCRIPT_PATH/build/src/site/lib/sentry/init.js.in" "$THIS_SCRIPT_PATH/build/src/site/lib/sentry/init.js"
  rm "$THIS_SCRIPT_PATH/build/src/site/lib/sentry/init.js.in"
}

function installer_server() {
  # We need to build in a tmp folder so that npm doesn't get confused by our
  # monorepo setup, and so we can copy the relevant node_modules in, because
  # we'll need them in order to build the deployable version.

  local PRODBUILDTEMP=`mktemp -d`
  echo "Preparing in $PRODBUILDTEMP"
  # Remove @keymanapp devDependencies because they won't install outside the
  # monorepo context
  cat package.json | "$JQ" \
    '. | del(.devDependencies."@keymanapp/resources-gosh") | del(.devDependencies."@keymanapp/keyman-version") | del(.dependencies."@keymanapp/developer-utils")' \
    > "$PRODBUILDTEMP/package.json"

  pushd "$PRODBUILDTEMP"
  npm install --omit=dev --omit=optional
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  rm -f node_modules/ngrok/bin/ngrok.exe
  popd

  # @keymanapp/keyman-version is required in build/ now but we need to copy it in manually
  mkdir -p "$PRODBUILDTEMP/node_modules/@keymanapp/"
  cp -R "$KEYMAN_ROOT/node_modules/@keymanapp/keyman-version/" "$PRODBUILDTEMP/node_modules/@keymanapp/"
  cp -R "$KEYMAN_ROOT/node_modules/@keymanapp/developer-utils/" "$PRODBUILDTEMP/node_modules/@keymanapp/"

  # We'll build in the $KEYMAN_ROOT/developer/bin/server/ folder
  rm -rf "$KEYMAN_ROOT/developer/bin/server/"
  mkdir -p "$KEYMAN_ROOT/developer/bin/server/build/"
  cp -R build/* "$KEYMAN_ROOT/developer/bin/server/build/"
  cp -R "$PRODBUILDTEMP"/* "$KEYMAN_ROOT/developer/bin/server/"
  rm -rf "$PRODBUILDTEMP"
}

function test_server() {
  # eslint .
  tsc --build test
  # c8 --reporter=lcov --reporter=text
  mocha
}

builder_run_action clean:server        clean_server
builder_run_action configure:server    configure_server
builder_run_action build:server        build_server
builder_run_action build:addins        build_addins
builder_run_action test:server         test_server
# builder_run_action test:addins       # no op
builder_run_action installer:server    installer_server

# TODO: consider 'watch'
# function watch_server() {
#   tsc-watch --onSuccess "node --inspect ." --onFailure "node --inspect ."
# }
# builder_run_action watch:server      watch_server
