#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# Calls resource script to perform the dSYM upload

source "$KEYMAN_ROOT/resources/build/mac/xcode-utils.inc.sh"
phaseSentryDsymUpload "keyman-ios"