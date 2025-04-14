# shellcheck shell=bash
# no hashbang for .inc.sh

. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"

_add_build_args() {
  local var=$1
  local default_var=$2
  local name=$3
  local value

  value="${!var:=${!default_var:-}}"

  build_args+=(--build-arg="${var}=${value}")

  if [[ -n "${build_version:-}" ]]; then
    build_version="${build_version}-${name:-}${value}"
  else
    build_version="${name}${value}"
  fi
}

# Convert the parameters passed in to the script to build arguments
# that form the docker image name
# Returns:
#   build_args: array of build arguments
#   build_version: version string of the docker image
convert_parameters_to_args() {
  build_args=()
  build_version=
  local required_node_version keyman_default_distro
  # shellcheck disable=SC2034
  required_node_version="$(_print_expected_node_version)"
  # shellcheck disable=SC2034
  keyman_default_distro="ubuntu"

  _add_build_args DISTRO                       keyman_default_distro                    ""
  _add_build_args DISTRO_VERSION               KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER  ""
  _add_build_args JAVA_VERSION                 KEYMAN_VERSION_JAVA                      java
  _add_build_args REQUIRED_NODE_VERSION        required_node_version                    node
  _add_build_args REQUIRED_EMSCRIPTEN_VERSION  KEYMAN_MIN_VERSION_EMSCRIPTEN            emsdk

  if [[ -n "${BASE_VERSION:-}" ]]; then
    build_args+=(--build-arg="BASE_VERSION=${BASE_VERSION}")
  else
    build_args+=(--build-arg="BASE_VERSION=${build_version}")
  fi
}

check_for_default_values() {
  if [[ -z "${DISTRO_VERSION:-}" ]] && [[ -z "${JAVA_VERSION:-}" ]]; then
    is_default_values=true
  else
    is_default_values=false
  fi
}

# Check if a specific Ubuntu or Java version is specified
is_default_values() {
  ${is_default_values}
}
