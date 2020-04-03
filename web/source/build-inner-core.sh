#! /bin/bash
# 
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

# Generates a linkable TS file; defined in resources/build-utils.sh.
exportEnvironmentDefinitionTS

# Compile web-inner-core.
npm run tsc -- -p source/inner-core.tsconfig.json