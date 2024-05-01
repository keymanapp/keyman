# builder.inc.sh
#
# Wrapper for Builder scripts. Do not confuse this with
# /resources/builder.inc.sh which has the full implementation of builder scripts
#
# This script should only be sourced where builder_ functions are used, and will
# report an error if builder_parse is never called.


# Note: set -eu and SHLVL are deliberately set both here and in build-utils.sh
# Exit on command failure and when using unset variables:
set -eu
# Prevents 'clear' on exit of mingw64 bash shell
SHLVL=0


function __builder_find_keyman_root() {
  # We don't need readlink here because our standard script prolog does a
  # readlink -f already so we will have already escaped from any symlinks
  # But we still need to canonicalize paths to remove ../../..
  KEYMAN_ROOT="${BASH_SOURCE[0]%/*/*/*}"
  KEYMAN_ROOT="$( cd "$KEYMAN_ROOT" && echo "$PWD" )"
  readonly KEYMAN_ROOT
}

if [[ -z ${KEYMAN_ROOT+x} ]]; then
  __builder_find_keyman_root
fi

__builder_not_a_builder_script() {
  if ! _builder_has_function_been_called builder_describe; then
    builder_echo warning "builder_describe was never called; script is not a valid builder script"
    exit 1
  fi
  if ! _builder_has_function_been_called builder_parse; then
    builder_echo warning "builder_parse was never called; script is not a valid builder script"
    exit 1
  fi
}

trap __builder_not_a_builder_script exit

# This will also import /resources/builder.inc.sh

# TODO: rename build-utils.sh to utils.sh

. "$KEYMAN_ROOT/resources/build/build-utils.sh"

# All builder scripts start in their own folder
cd "$THIS_SCRIPT_PATH"
