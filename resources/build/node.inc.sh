# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

[[ -z ${_utils_inc_sh+x} ]] && source "${KEYMAN_ROOT}/resources/build/utils.inc.sh"

# Verifies that node is installed, and installs npm packages, but only once per
# build invocation
node_select_version_and_npm_ci() {
  # We'll piggy-back on the builder module dependency build state to determine
  # if npm ci has been called in the current script invocation. Adding the
  # prefix /external/ to module name in order to differentiate between this and
  # internal modules (although it is unlikely to ever collide!); we will also
  # use this pattern for other similar external dependencies in future. These
  # functions are safe to call even in a non-builder context (they do nothing or
  # return 1 -- not built)
  if builder_has_module_been_built /external/npm-ci; then
    return 0
  fi
  builder_set_module_has_been_built /external/npm-ci

  # If we are on CI environment, automatically select a node version with nvm
  # Also, a developer can set KEYMAN_USE_NVM variable to get this behaviour
  # automatically too (see /docs/build/node.md)
  if [[ "$KEYMAN_VERSION_ENVIRONMENT" != local || ! -z "${KEYMAN_USE_NVM+x}" ]]; then
    # For npm publishing, we currently need to use an alternate version of node
    # that includes npm 11.5.1 or later (see #15040). Once we move to node v24,
    # we can probably remove this check
    if [[ -z "${KEYMAN_CI_SKIP_NVM+x}" ]]; then
      _node_select_version_with_nvm
    fi
  fi

  # Check if Node.JS/npm is installed.
  type npm >/dev/null ||\
    builder_die "Build environment setup error detected!  Please ensure Node.js is installed!"

  pushd "$KEYMAN_ROOT" > /dev/null

  offline_param=
  if builder_try_offline; then
    builder_echo "Trying offline build"
    offline_param=--prefer-offline
  fi
  try_multiple_times npm ${offline_param} ci

  popd > /dev/null
}

_node_print_expected_version() {
  "$JQ" -r '.engines.node' "$KEYMAN_ROOT/package.json"
}

# Use nvm to select a node version according to package.json
# see /docs/build/node.md
_node_select_version_with_nvm() {
  local REQUIRED_NODE_VERSION  CURRENT_NODE_VERSION

  REQUIRED_NODE_VERSION="$(_node_print_expected_version)"
  if [[ -z "$REQUIRED_NODE_VERSION" ]]; then
    builder_die "Could not find expected Node.js version in $KEYMAN_ROOT/package.json"
  fi

  if builder_is_windows; then
    CURRENT_NODE_VERSION="$(node --version)"
    if [[ "${CURRENT_NODE_VERSION}" != "v${REQUIRED_NODE_VERSION}" ]]; then
      start //wait //b nvm install "${REQUIRED_NODE_VERSION}"
      start //wait //b nvm use "${REQUIRED_NODE_VERSION}"
    fi
  else
    # launch nvm in a sub process, see _builder_nvm.sh for details
    "${KEYMAN_ROOT}/resources/build/_builder_nvm.sh" "${REQUIRED_NODE_VERSION}"

    if ! echo "${PATH}" | grep -qF "${HOME}/.keyman/node"; then
      export PATH="${HOME}/.keyman/node:${PATH}"
    fi
  fi

  # Now, check that the node version is correct, on all systems

  # Note: On windows, `nvm use` and `nvm install` always return success.
  # https://github.com/coreybutler/nvm-windows/issues/738

  # note the 'v' prefix that node emits (and npm doesn't!)
  CURRENT_NODE_VERSION="$(node --version)"
  if [[ "$CURRENT_NODE_VERSION" != "v$REQUIRED_NODE_VERSION" ]]; then
    builder_die "Attempted to select node.js version $REQUIRED_NODE_VERSION but found $CURRENT_NODE_VERSION instead"
  fi
}
