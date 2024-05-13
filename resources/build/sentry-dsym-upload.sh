#!/usr/bin/env bash

# Calls resource script to perform the dSYM upload

source "$KEYMAN_ROOT/resources/build/xcode-utils.sh"
phaseSentryDsymUpload "keyman-ios"