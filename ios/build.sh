#!/usr/bin/env bash
#
# Build Keyman Engine for iPhone and iPad, Keyman for iPhone and iPad, OEM FirstVoices iOS app,
# Samples: KMSample1 and KMSample2
#
# OEM FirstVoices iOS app requires the following env vars set:
# RELEASE_OEM should be set
# RELEASE_OEM_FIRSTVOICES should be true

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

#
# Restrict available OEM publish targets to those that can be built on the current system
#

oemtargets=()
if [[ ! -z "${RELEASE_OEM+x}" ]]; then
  if [[ "${RELEASE_OEM_FIRSTVOICES-false}" = true ]]; then
    oemtargets+=(":fv=../oem/firstvoices/ios   Builds OEM FirstVoices for iOS platforms")
  fi
fi

builder_describe "Builds Keyman Engine and the Keyman app for use on iOS devices - iPhone and iPad." \
  "@/resources/tools/check-markdown  test:help" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":engine                      Builds KeymanEngine.xcframework, usable by our main app and by third-party apps" \
  ":app=keyman                  Builds the Keyman app for iOS platforms" \
  ":help                        Online documentation" \
  ":sample1=samples/KMSample1   Builds the first KeymanEngine sample app" \
  ":sample2=samples/KMSample2   Builds the second KeymanEngine sample app" \
  "${oemtargets[@]}" \
  "--sim-artifact+              Also outputs a simulator-friendly test artifact corresponding to the build"

builder_parse "$@"

builder_run_child_actions clean configure build test

function do_test_help() {
  check-markdown  "$KEYMAN_ROOT/ios/docs/help"
  check-markdown  "$KEYMAN_ROOT/ios/docs/engine"
}

builder_run_action        test:help    do_test_help
