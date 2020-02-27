#!/bin/bash

BASE_DIR=`pwd`

function generateStringFiles() {
  # Checks all project files (under current directory) for
  # i18n-able strings.
  find . -name \*.swift | tr '\n' '\0' | xargs -0 genstrings -o en.lproj
}

KMEA_BASE=engine/KMEI/KeymanEngine
APP_BASE=keyman/Keyman/Keyman

cd $KMEA_BASE

generateStringFiles

cd $BASE_DIR
cd $APP_BASE

generateStringFiles

cd $BASE_DIR