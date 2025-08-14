#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# Runs a script using the homebrew version of bash from within an xcode script;
# we need this because xcode does not inherit paths from user's terminal
# environment.

echo "xcode-wrap: wrap script for arch $(arch)"
if [[ $(arch) == i386 ]] && [[ -f /usr/local/bin/bash ]]; then
  /usr/local/bin/bash -l "$@" || exit $?
elif [[ $(arch) == arm64 ]] && [[ -f /opt/homebrew/bin/bash ]]; then
  /opt/homebrew/bin/bash -l "$@" || exit $?
else
  >&2 echo "xcode-wrap: Could not start build due to missing homebrew bash"
  exit 55
fi