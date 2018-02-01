#!/bin/sh
# Build KeyboardHarness test app

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

echo Build KMEA
cd ../../KMEA
./build.sh
cd ../Tests/KeyboardHarness
cp ../keyman-engine.aar app/libs/

echo Build KeyboardHarness test app
./gradlew clean build
