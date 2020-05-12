#!/bin/bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$(dirname "$THIS_SCRIPT")/build/jq.inc.sh"

display_usage ( ) {
  echo ""
  echo "package-utils.sh accepts one of the following options:"
  echo ""
  echo "  add-package          to insert a path to a folder with a package.json"
  echo "                       for lerna-based management"
  echo ""
  echo "  remove-package       to remove a previously-inserted package path"
  echo "" 
  exit 1
}

PACKAGE_SOURCE='node_modules'
PACKAGE_STASH='__node_modules'

add_package() {
  pushd $KEYMAN_ROOT

  lerna_data=`cat lerna.json`
  new_data=`echo "$lerna_data" | $JQ ".packages += [\"$1\"]"`

  if [ $? != 0 ]; then
    exit 1
  fi

  cp lerna.json lerna.json.backup
  echo "$new_data" > lerna.json

  popd
}

remove_package() {
  pushd $KEYMAN_ROOT

  lerna_data=`cat lerna.json`
  new_data=`echo "$lerna_data" | $JQ "del(.packages[] | select(. ==\"$1\"))"`
  if [ $? != 0 ]; then
    exit 1
  fi

  cp lerna.json lerna.json.backup
  echo "$new_data" > lerna.json

  popd
}

# Parse args
# If no argument was provided, print help.
if [ $# -eq 0 ]; then
  display_usage
else
  case $1 in
    add-package)
      add_package $2
      ;;
    remove-package)
      remove_package $2
      ;;
    # Default case - display help
    *)
      display_usage
      ;;
  esac
fi