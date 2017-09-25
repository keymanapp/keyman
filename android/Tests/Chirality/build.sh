#!/bin/sh
# Build Chirality

echo Build KMEA
cd ../../KMEA
./build.sh
cd ../Tests/Chirality
cp ../keyman-engine.aar app/libs/

echo Build Chirality
./gradlew clean build
