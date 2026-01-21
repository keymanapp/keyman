#!/usr/bin/env bash
set -eu

"$(dirname "$0")/deb-packaging.tests.sh"
"$(dirname "$0")/verify_api.tests.sh"
"$(dirname "$0")/package-build.inc.tests.sh"
