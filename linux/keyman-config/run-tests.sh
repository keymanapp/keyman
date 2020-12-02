#!/bin/bash
PYTHONPATH=.:$PYTHONPATH

if [ -n "$TEAMCITY_VERSION" ]; then
    if ! pip3 list --format=columns | grep -q teamcity-messages; then
        pip3 install teamcity-messages
    fi
    python3 -m teamcity.unittestpy discover -s tests -p test_*.py
else
    python3 -m unittest discover -v -s tests -p test_*.py
fi
