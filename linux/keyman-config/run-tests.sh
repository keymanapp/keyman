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
  python3 -m teamcity.unittestpy discover -s tests -p test_*.py
else
  # shellcheck disable=SC2086
  python3 ${coverage:-} -m unittest discover -v -s tests -p test_*.py
fi

rm -rf "$XDG_CONFIG_HOME"
