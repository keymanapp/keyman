#!/usr/bin/env bash
# Step name: Upload new Keyman Linux help to help.keyman.com

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/resources/build" || exit 1

cd ../../../help.keyman.com || exit 1
git config credential.helper '!f() { sleep 1; echo "username=${GITHUB_USER}"; echo "password=${GITHUB_TOKEN}"; }; f'
cd "${REPO_ROOT}/resources/build" || exit 1
./help-keyman-com.sh linux
