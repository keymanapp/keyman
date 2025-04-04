#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/docker-images/docker-build.inc.sh"

builder_describe \
  "Build docker images" \
  ":android" \
  ":base" \
  ":core" \
  ":linux" \
  ":web" \
  "--distro=DISTRO                  The distribution to use for the base image "\
  "                                 (debian or ubuntu, default: ubuntu)" \
  "--distro-version=DISTRO_VERSION  The Ubuntu/Debian version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})" \
  "--no-cache                       Force rebuild of docker images" \
  "build                            Build docker images" \
  "test                             Test the docker images by running configure,build,test for all or the specified platforms"

builder_parse "$@"

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

_convert_parameters_to_build_args() {
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
  fi
}

_check_for_default_values() {
  if [[ -z "${DISTRO_VERSION:-}" ]] && [[ -z "${JAVA_VERSION:-}" ]]; then
    is_default_values=true
  else
    is_default_values=false
  fi
}

_is_default_values() {
  ${is_default_values}
}

build_action() {
  local platform=$1

  builder_echo debug "Building image for ${platform}"

  if [[ "${platform}" == "base" ]]; then
    # shellcheck disable=SC2154 # set by _convert_parameters_to_build_args
    docker pull "${DISTRO}:${DISTRO_VERSION}"
  elif [[ "${platform}" == "linux" ]]; then
    cp "${KEYMAN_ROOT}/linux/debian/control" "${platform}"
  fi

  if builder_has_option --no-cache; then
    OPTION_NO_CACHE="--no-cache"
  fi

  # shellcheck disable=SC2164
  cd "${platform}"
  # shellcheck disable=SC2248,SC2086
  docker build ${OPTION_NO_CACHE:-} -t "keymanapp/keyman-${platform}-ci:${build_version}" "${build_args[@]}" .
  # If the user didn't specify particular versions we will additionaly create an image
  # with the tag 'default'.
  if is_default_values; then
    builder_echo debug "Setting default tag for ${platform}"
    docker build -t "keymanapp/keyman-${platform}-ci:default" "${build_args[@]}" .
  fi
  # shellcheck disable=SC2164,SC2103
  cd -
  builder_echo success "Docker image 'keymanapp/keyman-${platform}-ci:${build_version}' built"
}

test_action() {
  local platform=$1

  builder_echo debug "Testing image for ${platform}"
  ./run.sh --distro "${DISTRO}" --distro-version "${DISTRO_VERSION}" \
    "${platform}" -- ./build.sh configure,build,test:"${platform}"
}

_check_for_default_values
_convert_parameters_to_build_args

if builder_has_action build; then
  build_action base
  builder_run_action build:android  build_action android
  builder_run_action build:core     build_action core
  builder_run_action build:linux    build_action linux
  builder_run_action build:web      build_action web
fi

builder_run_action test:core        test_action core
builder_run_action test:linux       test_action linux
builder_run_action test:web         test_action web
# Android uses artifacts from web, so it has to come after web
builder_run_action test:android     test_action android
