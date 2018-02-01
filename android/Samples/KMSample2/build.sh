#!/bin/sh
# Build KMSample2

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

echo Build KMSample2
./gradlew clean build
