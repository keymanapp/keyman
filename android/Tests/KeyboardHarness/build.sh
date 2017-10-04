#!/bin/sh
# Build KeyboardHarness test app

echo Build KMEA
cd ../../KMEA
./build.sh
cd ../Tests/KeyboardHarness
cp ../keyman-engine.aar app/libs/

echo Build KeyboardHarness test app
./gradlew clean build
