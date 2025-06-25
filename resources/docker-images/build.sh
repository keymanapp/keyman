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
  ":developer" \
  ":linux" \
  ":web" \
  "--distro=DISTRO                  The distribution to use for the base image (debian or ubuntu, default: ubuntu)" \
  "--distro-version=DISTRO_VERSION  The Ubuntu/Debian version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})" \
  "--no-cache                       Force rebuild of docker images" \
  "build                            Build docker images" \
  "test                             Test the docker images by running configure,build,test for all or the specified platforms"

builder_parse "$@"

build_action() {
  local platform=$1

  builder_echo debug "Building image for ${platform}"

  if [[ "${platform}" == "base" ]]; then
    # shellcheck disable=SC2154 # set by convert_parameters_to_args
    docker_wrapper pull "${DISTRO}:${DISTRO_VERSION}"
  elif [[ "${platform}" == "linux" ]]; then
    cp "${KEYMAN_ROOT}/linux/debian/control" "${platform}"
  fi

  if builder_has_option --no-cache; then
    OPTION_NO_CACHE="--no-cache"
  fi

  # shellcheck disable=SC2164
  cd "${platform}"
  export DOCKER_BUILDKIT=1
  # shellcheck disable=SC2248,SC2086
  docker_wrapper build ${OPTION_NO_CACHE:-} -t "keymanapp/keyman-${platform}-ci:${build_version}" "${build_args[@]}" .
  # If the user didn't specify particular versions we will additionaly create an image
  # with the tag 'default'.
  if is_default_values; then
    builder_echo debug "Setting default tag for ${platform}"
    docker_wrapper build -t "keymanapp/keyman-${platform}-ci:default" "${build_args[@]}" .
  fi
  # shellcheck disable=SC2164,SC2103
  cd -
  builder_echo success "Docker image 'keymanapp/keyman-${platform}-ci:${build_version}' built"
}

test_action() {
  local platform=$1

  builder_echo debug "Testing image for ${platform}"
  ./run.sh --distro "${DISTRO}" --distro-version "${DISTRO_VERSION}" \
    ":${platform}" -- ./build.sh configure,build,test:"${platform}"
}

check_buildx_available
check_for_default_values
convert_parameters_to_args

if builder_has_action build; then
  build_action base
  builder_run_action build:android    build_action android
  builder_run_action build:core       build_action core
  builder_run_action build:linux      build_action linux
  builder_run_action build:web        build_action web
  builder_run_action build:developer  build_action developer
fi

builder_run_action test:core        test_action core
builder_run_action test:linux       test_action linux
builder_run_action test:web         test_action web
# Android uses artifacts from web, so it has to come after web
builder_run_action test:android     test_action android
builder_run_action test:developer   test_action developer
