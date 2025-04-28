#!/bin/bash
PYTHONPATH=.:${PYTHONPATH}

XDG_CONFIG_HOME=$(mktemp --directory)
export XDG_CONFIG_HOME

if [[ -f /usr/libexec/ibus-memconf ]]; then
  export GSETTINGS_BACKEND=keyfile
fi

if [[ "$1" == "--coverage" ]]; then
  coverage="-m coverage run --source=. --data-file=build/.coverage"
fi

if [[ -n "${TEAMCITY_VERSION}" ]]; then
  if ! pip3 list --format=columns | grep -q teamcity-messages; then
      if [[ -n "${TEAMCITY_PLATFORM}" ]] || [[ -n "${DOCKER_RUNNING}" ]]; then
        # Ubuntu 24.04+ prevents mixing pip and system packages and wants us
        # to use a virtualenv. However, that causes other problems, so we
        # override this check only if we're running in a TC agent or docker
        # so that we don't accidentally break the local system.
        PIP_ARGS="--break-system-packages"
      fi
      # shellcheck disable=SC2086
      pip3 install --user ${PIP_ARGS:-} teamcity-messages
  fi
  test_module=teamcity.unittestpy
else
  test_module=unittest
  extra_opts=-v
fi

# shellcheck disable=SC2086
python3 ${coverage:-} -m "${test_module:-}" discover ${extra_opts:-} -s tests/ -p "*_tests.py"

rm -rf "${XDG_CONFIG_HOME}"
