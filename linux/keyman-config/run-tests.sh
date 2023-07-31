#!/bin/bash
PYTHONPATH=.:$PYTHONPATH

XDG_CONFIG_HOME=$(mktemp --directory)
export XDG_CONFIG_HOME

if [ -f /usr/libexec/ibus-memconf ]; then
  export GSETTINGS_BACKEND=keyfile
fi

if [ -n "$TEAMCITY_VERSION" ]; then
    if ! pip3 list --format=columns | grep -q teamcity-messages; then
        pip3 install teamcity-messages
    fi
    python3 -m teamcity.unittestpy discover -s tests -p test_*.py
else
    python3 -m unittest discover -v -s tests -p test_*.py
fi

rm -rf "$XDG_CONFIG_HOME"
