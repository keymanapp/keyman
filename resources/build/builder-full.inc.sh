# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Wrapper for Builder scripts. Do not confuse this with
# /resources/builder.inc.sh which has the full implementation of builder scripts
#
# This script should only be sourced where builder_ functions are used, and will
# report an error if builder_parse is never called.


# Note: set -eu and SHLVL are deliberately set both here and in builder-basic.inc.sh
# Exit on command failure and when using unset variables:
set -eu
# Prevents 'clear' on exit of mingw64 bash shell
SHLVL=0

function __builder_full_not_a_builder_script() {
  if ! _builder_has_function_been_called builder_describe; then
    builder_echo warning "builder_describe was never called; script is not a valid builder script"
    exit 1
  fi
  if ! _builder_has_function_been_called builder_parse; then
    builder_echo warning "builder_parse was never called; script is not a valid builder script"
    exit 1
  fi
}

trap __builder_full_not_a_builder_script exit

# This will also import /resources/builder.inc.sh
. "${BASH_SOURCE[0]%/*}/builder-basic.inc.sh"

# All full builder scripts start in their own folder
cd "$THIS_SCRIPT_PATH"
