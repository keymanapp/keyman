#!/usr/bin/env bash

echo "wrap script for arch $(arch)"
if [[ $(arch) == i386 ]] && [[ -f /usr/local/bin/bash ]]; then
  /usr/local/bin/bash -l "$@" || exit $?
elif [[ $(arch) == arm64 ]] && [[ -f /opt/homebrew/bin/bash ]]; then
  /opt/homebrew/bin/bash -l "$@" || exit $?
else
  >&2 echo "Could not start build due to missing homebrew bash"
  exit 55
fi 