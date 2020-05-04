#!/bin/bash
# Get the version number from VERSION.md
# When building from a Linux source package, `VERSION.md` is in the
# same directory as this script.
VERSIONFILE=VERSION.md
[ -f ../../../VERSION.md ] && VERSIONFILE=../../../VERSION.md
cat $VERSIONFILE
