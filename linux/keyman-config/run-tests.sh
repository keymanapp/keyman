#!/bin/bash
PYTHONPATH=.:$PYTHONPATH

XDG_CONFIG_HOME=$(mktemp --directory)
export XDG_CONFIG_HOME

if [ -f /usr/libexec/ibus-memconf ]; then
  export GSETTINGS_BACKEND=keyfile
fi

if [ "$1" == "--coverage" ]; then
  coverage="-m coverage run --source=. --data-file=build/.coverage"
fi

if [ -n "$TEAMCITY_VERSION" ]; then
  if ! pip3 list --format=columns | grep -q teamcity-messages; then
      pip3 install teamcity-messages
  fi
  test_module=teamcity.unittestpy
else
  test_module=unittest
  extra_opts=-v
fi

# shellcheck disable=SC2086
python3 ${coverage:-} -m ${test_module:-} discover ${extra_opts:-} -s tests -p test_*.py

rm -rf "$XDG_CONFIG_HOME"
