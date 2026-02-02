#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"
. "${KEYMAN_ROOT}/resources/docker-images/docker-build.inc.sh"
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"

################################ Main script ################################

builder_describe \
  "Run build.sh script inside of a docker image. Pass the build script and parameters after --." \
  ":android" \
  ":core" \
  ":developer" \
  ":linux" \
  ":web" \
  "run+                             Run command in docker image" \
  "--distro=DISTRO                  The distribution (debian or ubuntu, default: ubuntu)" \
  "--distro-version=DISTRO_VERSION  The Ubuntu/Debian version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})" \
  "--registry=REGISTRY              The container registry to keep the images (default: ghcr.io/keymanapp)" \
  "--username=REGISTRY_USERNAME     The user for the container registry (default: keymanapp)" \
  "--password=REGISTRY_PASSWORD     The password for the container registry user (default: password)" \
  "--remote-debug                   Expose the container's port 2345 for remote debugging"

builder_parse "$@"

check_for_default_values
convert_parameters_to_args
setup_docker
setup_container_registry

if is_default_values; then
  image_version=default
  build_dir=default
else
  image_version=${build_version:-}
  build_dir="${DISTRO:-}-${DISTRO_VERSION:-}"
fi

run_android() {
  docker_wrapper run "${DOCKER_RUN_ARGS[@]}" -i --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "${registry_slash}keymanapp/keyman-android-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_core() {
  docker_wrapper run "${DOCKER_RUN_ARGS[@]}" -i --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "${registry_slash}keymanapp/keyman-core-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_developer() {
  docker_wrapper run "${DOCKER_RUN_ARGS[@]}" -i --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "${registry_slash}keymanapp/keyman-developer-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_linux() {
  mkdir -p "${KEYMAN_ROOT}/linux/build/docker-linux/${build_dir}"
  mkdir -p "${KEYMAN_ROOT}/linux/keyman-system-service/build/docker-linux/${build_dir}"
  mkdir -p "${KEYMAN_ROOT}/linux/mcompile/keymap/build/docker-linux/${build_dir}"
  docker_wrapper run "${DOCKER_RUN_ARGS[@]}" -i --privileged --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    -v "${KEYMAN_ROOT}/linux/build/docker-linux/${build_dir}":/home/build/build/linux/build \
    -v "${KEYMAN_ROOT}/linux/keyman-system-service/build/docker-linux/${build_dir}":/home/build/build/linux/keyman-system-service/build \
    -v "${KEYMAN_ROOT}/linux/mcompile/keymap/build/docker-linux/${build_dir}":/home/build/build/linux/mcompile/keymap/build \
    -e DESTDIR=/tmp \
    "${registry_slash}keymanapp/keyman-linux-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_web() {
  docker_wrapper run "${DOCKER_RUN_ARGS[@]}" -i --privileged --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "${registry_slash}keymanapp/keyman-web-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

mkdir -p "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}"

builder_run_action run:android    run_android
builder_run_action run:core       run_core
builder_run_action run:developer  run_developer
builder_run_action run:linux      run_linux
builder_run_action run:web        run_web
