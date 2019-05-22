#!/bin/bash

set -e
# set -x
# set -u <-- difficult to test if variable is set this way...

./kmbuild.sh $*

if [ ! -z "$RELEASE_OEM" ]; then
  pushd ../oem/firstvoices/ios > /dev/null
  ./build.sh $*
  popd > /dev/null
fi
