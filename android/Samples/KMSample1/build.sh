#!/bin/sh
# Build KMSample1

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

echo Build KMSample1
./gradlew clean build
