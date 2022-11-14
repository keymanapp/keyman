#!/usr/bin/env bash
#
# Compiles Keyman Developer Server for deployment
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE
EX_USAGE=64

pushd "$(dirname "$THIS_SCRIPT")"

# Build the main script.
build () {
  npm run build || fail "Could not build top-level JavaScript file."
}

display_usage ( ) {
  echo "Usage: $0 [--production|--watch] [--test|--tdd]"
  echo "       $0 --help"
  echo
  echo "  --help                      displays this screen and exits"
  echo "  --production, -p            builds production release in build/"
  echo "  --skip-package-install, -S  don't run npm install (not valid with --production)"
  echo "  --test, -t                  runs unit tests after building"
  #echo "  --watch, -w                 builds dev server in watch mode"
  echo "  --tdd                       runs unit tests WITHOUT building"
  echo "  --no-build-addins           don't build/copy Win32 addins"
  echo "  --no-build-kmw              don't build KeymanWeb"
  echo "  --no-copy-kmw               don't copy KeymanWeb"
}

################################ Main script ################################

run_tests=0
production=0
build=1
install_dependencies=1
build_keymanweb=1
copy_keymanweb=1
build_addins=1

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h|-?)
      display_usage
      exit
      ;;
    --production|-p)
      production=1
      ;;
    --skip-package-install|-S)
      install_dependencies=0
      ;;
    --no-build-kmw)
      build_keymanweb=0
      ;;
    --no-build-addins)
      build_addins=0
      ;;
    --no-copy-kmw)
      copy_keymanweb=0
      ;;
    --test)
      run_tests=1
      ;;
    --tdd)
      run_tests=1
      build=0
      install_dependencies=0
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit $EX_USAGE
  esac
  shift # past the processed argument
done

# ----------------------------------------
# Install dependencies
# ----------------------------------------

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"

if (( install_dependencies )) ; then
  verify_npm_setup
  npm install --production=false
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  rm -f node_modules/ngrok/bin/ngrok.exe
fi

set_npm_version || fail "Setting version failed."

# ----------------------------------------
# Rebuild and bundle addins
# ----------------------------------------

if (( build_addins )); then
  . ./build-addins.inc.sh

  # If we have an x64 version of node installed
  if [[ $(isNodeX64) ]]; then
    build_addins
  fi

  # Build with the Keyman Developer x86 version of node
  PATH="$KEYMAN_ROOT/developer/inst/dist:$PATH" build_addins
fi

# ----------------------------------------
# Build and bundle KeymanWeb
# ----------------------------------------

if (( build_keymanweb )); then
  pushd "$KEYMAN_ROOT/web/source"
  ./build.sh -no_minify
  popd

  # TODO: This is gross, but lerna is stripping out @keymanapp/resources-gosh
  # from our local node-modules folder during the KeymanWeb build, and I don't
  # want to try and figure out why just at this minute.
  #
  # This causes `npm run postbuild` (which also happens after `npm run build`)
  # to fall over because `gosh` cannot be found. An alternative fix was to
  # change the postbuild script from:
  #
  #   npx gosh ./postbuild.sh
  #
  # to:
  #
  #   npx -p file:../../resources/gosh -q gosh ./postbuild.sh
  #
  # Which is also gross and should not be required!
  #
  npm install --production=false
fi

if (( copy_keymanweb )); then
  SRC="$KEYMAN_ROOT/web/release/unminified"
  DST="$(dirname "$THIS_SCRIPT")/src/site/resource"

  rm -rf "$DST"
  mkdir -p "$DST/osk"
  mkdir -p "$DST/ui"
  cp "$SRC/web/"*.js "$SRC/web/"*.js.map "$DST/"
  cp -R "$SRC/web/osk/"* "$DST/osk/"
  cp -R "$SRC/web/ui/"* "$DST/ui/"
  cp "$KEYMAN_ROOT/web/LICENSE" "$DST/"
  cp "$KEYMAN_ROOT/web/README.md" "$DST/"
fi

# ----------------------------------------
# Build the project
# ----------------------------------------

npm run build || fail "Compilation failed."
echo "Typescript compilation successful."

# ----------------------------------------
# Unit tests
# ----------------------------------------

if (( run_tests )); then
  npm test || fail "Tests failed"
fi

# ----------------------------------------
# Deploy to dist/
# ----------------------------------------

if (( production )) ; then
  # We'll build in the build/ folder
  rm -rf build/
  mkdir build/
  cp -R dist/ package.json package-lock.json build/
  cd build/
  npm install --omit=dev --omit=optional
  # See https://github.com/bubenshchykov/ngrok/issues/254, https://github.com/bubenshchykov/ngrok/pull/255
  rm -f node_modules/ngrok/bin/ngrok.exe
  cd ..
fi
