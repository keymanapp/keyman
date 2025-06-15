#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

source "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Installation files for Keyman Engine for Windows" \
  clean configure build test publish

# NOTE: not using deps here because we will only do this in the 'publish' phase
# after all other builds complete

builder_describe_outputs \
  publish       /windows/release/${KEYMAN_VERSION}/keymanengine-${KEYMAN_VERSION}.msm

builder_parse "$@"

. "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/wix.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/zip.inc.sh"

# In dev environments, we'll hack the tier to alpha; CI sets this for us in real builds.
if [[ -z ${KEYMAN_TIER+x} ]]; then
  KEYMAN_TIER=alpha
fi

#-------------------------------------------------------------------------------------------------------------------

function do_publish() {
  if_release_build_level verify-program-signatures

  #
  # Build the installation archive
  #
  "$WIXCANDLE" -dKEYMAN_VERSION=$KEYMAN_VERSION_WIN -dRELEASE=$KEYMAN_VERSION_RELEASE -ext WixUtilExtension keymanengine.wxs components.wxs

  # warning 1072 relates to Error table defined by WixUtilExtension. Doesn't really affect us.
  "$WIXLIGHT" \
    -sw1072 \
    -ext WixUtilExtension \
    "$WIXLIGHTCOMPRESSION" \
    keymanengine.wixobj components.wixobj \
    -o keymanengine.msm

  #
  # Sign the installation archive
  #
  wrap-signcode //d "Keyman Engine for Windows" keymanengine.msm

  copy-installer
}

function copy-installer() {
  builder_heading copy-installer
  mkdir -p "$KEYMAN_ROOT/windows/release/${KEYMAN_VERSION}"
  cp keymanengine.msm "$KEYMAN_ROOT/windows/release/${KEYMAN_VERSION}/keymanengine-${KEYMAN_VERSION}.msm"
}

function verify-program-signatures() {
  builder_heading verify-program-signatures

  verify-all-executable-signatures-in-folder "$KEYMAN_ROOT/windows/bin/engine"
}

function test-releaseexists() {
  if [[ -d "$KEYMAN_ROOT/windows/release/${KEYMAN_VERSION}" ]]; then
    builder_die "Release ${KEYMAN_VERSION} already exists. Delete it or update VERSION.md and try again"
  fi
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean     rm -f *.msm *.wixobj *.log *.wixpdb
# builder_run_action configure do_configure
# builder_run_action build     do_build
# builder_run_action test      do_test
builder_run_action publish   do_publish
