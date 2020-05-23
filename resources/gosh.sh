#!/bin/bash

# This script goes with gosh.bat and gosh.js. It may be helpful for launching
# a platform independent script "gosh foo.sh ..." where "gosh" will be interpreted
# as gosh.bat on Windows and this script on other platforms.

"$1" "${@:2}"
