#! /bin/bash
# 
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
KEYMAN_ROOT="$(dirname "$THIS_SCRIPT")/../../../../.."
. "$KEYMAN_ROOT/resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

verify_npm_setup

# Generates a linkable TS file; defined in resources/build-utils.sh.
exportEnvironmentDefinitionTS

# Compile web's `keyboard-processor` module.
npm run tsc -- -p src/tsconfig.json