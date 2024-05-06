#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"

builder_describe \
  "Build docker images" \
  ":android" \
  ":base" \
  ":core" \
  ":linux" \
  ":web" \
  "--ubuntu-version=UBUNTU_VERSION  The Ubuntu version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})" \
  "--node=NODE_MAJOR                Node version (default: ${KEYMAN_MIN_VERSION_NODE_MAJOR})" \
  "--emscripten=EMSCRIPTEN_VERSION  Emscripten version (default: ${KEYMAN_MIN_VERSION_EMSCRIPTEN})" \
  "--no-cache                       Force rebuild of docker images" \
  "build"

builder_parse "$@"

_add_build_args() {
  local var=$1
  local default_var=$2
  local name=$3
  local value

  if [[ -n "${!var:-}" ]]; then
    value="${!var}"
  else
    value="${!default_var:-}"
  fi

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

  _add_build_args UBUNTU_VERSION     KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER  ""
  _add_build_args JAVA_VERSION       KEYMAN_VERSION_JAVA                      java
  _add_build_args NODE_MAJOR         KEYMAN_MIN_VERSION_NODE_MAJOR            node
  _add_build_args EMSCRIPTEN_VERSION KEYMAN_MIN_VERSION_EMSCRIPTEN            emscr

  if [[ -n "${BASE_VERSION:-}" ]]; then
    build_args+=(--build-arg="BASE_VERSION=${BASE_VERSION}")
  fi
}

_is_default_values() {
  [[ -z "${UBUNTU_VERSION:-}" ]] && [[ -z "${JAVA_VERSION:-}" ]] && \
    [[ -z "${NODE_MAJOR:-}" ]] && [[ -z "${EMSCRIPTEN_VERSION:-}" ]]
}

build_action() {
  local platform=$1

  builder_echo debug "Building image for ${platform}"

  _convert_parameters_to_build_args

  if [[ "${platform}" == "base" ]]; then
    docker pull --platform "amd64" "ubuntu:${UBUNTU_VERSION:-${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER}}"
  ### TMP code
  elif [[ "${platform}" == "core" ]]; then
    cp "${KEYMAN_ROOT}/linux/debian/control" "${platform}"
  ### TMP END
  elif [[ "${platform}" == "linux" ]]; then
    cp "${KEYMAN_ROOT}/linux/debian/control" "${platform}"
  fi

  if builder_has_option --no-cache; then
    OPTION_NO_CACHE="--no-cache"
  fi

  cd "${platform}" || true
  # shellcheck disable=SC2248
  docker build ${OPTION_NO_CACHE:-} -t "keymanapp/keyman-${platform}-ci:${build_version}" "${build_args[@]}" .
  # If the user didn't specify particular versions we will additionaly create an image
  # with the tag 'default'.
  if _is_default_values; then
    builder_echo debug "Setting default tag for ${platform}"
    docker build . -t "keymanapp/keyman-${platform}-ci:default" "${build_args[@]}"
  fi
  cd - || true
  builder_echo success "Docker image 'keymanapp/keyman-${platform}-ci:${build_version}' built"
}

if builder_has_action build; then
  build_action base
  BASE_VERSION="${build_version}"
  builder_run_action build:android  build_action android
  builder_run_action build:core     build_action core
  builder_run_action build:linux    build_action linux
  builder_run_action build:web      build_action web
fi
