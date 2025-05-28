#!/bin/bash
export PYTHONPATH=.:${PYTHONPATH}

XDG_CONFIG_HOME=$(mktemp --directory)
export XDG_CONFIG_HOME

if [[ -f /usr/libexec/ibus-memconf ]]; then
  export GSETTINGS_BACKEND=keyfile
fi

if [[ "$1" == "--coverage" ]]; then
  coverage="-m coverage run --source=. --data-file=build/.coverage"
fi

if [[ -n "${TEAMCITY_GIT_PATH}" ]]; then
  PYTHONPATH=$(dirname "$0")/../tools:${PYTHONPATH}

  if ! pip3 list --format=columns | grep -q teamcity-messages; then
      if [[ -n "${TEAMCITY_PLATFORM}" ]] || [[ -n "${DOCKER_RUNNING}" ]]; then
        # Ubuntu 24.04+ prevents mixing pip and system packages and wants us
        # to use a virtualenv. However, that causes other problems, so we
        # override this check only if we're running in a TC agent or docker
        # so that we don't accidentally break the local system.
        # Note: it is intentional that we use TEAMCITY_GIT_PATH as well
        # as TEAMCITY_PLATFORM here so that it is possible to simulate
        # running in a TC agent and still not break your local system
        # (run in a docker container or explicitly set TEAMCITY_PLATFORM
        # to cause the script to install the package).
        PIP_ARGS="--break-system-packages"
      fi
      # shellcheck disable=SC2086
      pip3 install --user ${PIP_ARGS:-} teamcity-messages
  fi
  test_module=teamcity_testrunner.unittestpy
  echo "##teamcity[testStarted name='|[keyman-config|] Running unit tests']"
  echo "##teamcity[flowStarted flowId='unit_tests']"
else
  test_module=unittest
  extra_opts=-v
fi

# shellcheck disable=SC2086
python3 ${coverage:-} -m "${test_module:-}" discover ${extra_opts:-} -s tests/ -p "*_tests.py"

if [[ -n "${TEAMCITY_GIT_PATH}" ]]; then
  echo "##teamcity[flowFinished flowId='unit_tests']"
  echo "##teamcity[testFinished name='|[keyman-config|] Finished running unit tests']"
fi
rm -rf "${XDG_CONFIG_HOME}"
