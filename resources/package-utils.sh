#!/bin/bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
## END STANDARD BUILD SCRIPT INCLUDE
. "$(dirname "$THIS_SCRIPT")/build/jq.inc.sh"

display_usage ( ) {
  echo ""
  echo "package-utils.sh accepts one of the following options:"
  echo ""
  echo "  clean-external-deps  to erase the node_modules folder without erasing links."
  echo "                       Internally uses both link-stash and link-unstash."
  echo ""
  echo "  link-stash           to 'stash' the package's local links in a temporary"
  echo "                       directory."
  echo ""
  echo "  link-unstash         reverses 'link-stash', restoring the links to their"
  echo "                       original location and removing the temp directory."
  echo ""
  echo "  list-links           outputs the list of local dependencies used by the"
  echo "                       current package.json."
  echo "" 
  exit 1
}

PACKAGE_SOURCE='node_modules'
PACKAGE_STASH='__node_modules'

# Stores all @keymanapp package dependency/dev-dependency names to the 'packages' array.
get_local_dependencies() {
  localDeps=`cat package.json | $JQ -r ".dependencies, .devDependencies | to_entries | map(select(.key | match(\"@keymanapp/\";\"i\"))) | map(.key) | .[]"`

  # Sadly, we can't rely on the bash script `readlines` command - not all shells  (eg: macOS) support it!
  # This will have the same net effect.
  IFS=$'\n'
  packages=(${localDeps}); # Convert the output $localDeps string into a script-friendly array.
  unset IFS
}

stash_package_links() {
  get_local_dependencies

  mkdir -p "$PACKAGE_STASH/@keymanapp"
  for dep in "${packages[@]}"; do
    if [ "${QUIET:-false}" = false ]; then
      echo "Stashing link-based dependency: $dep"
    fi
    mv "$PACKAGE_SOURCE/$dep" "$PACKAGE_STASH/@keymanapp"
  done
}

unstash_package_links() {
  get_local_dependencies

  mkdir -p "$PACKAGE_SOURCE/@keymanapp"
  for dep in "${packages[@]}"; do
    if [ "${QUIET:-false}" = false ]; then
      echo "Restoring link-based dependency: $dep"
    fi
    mv "$PACKAGE_STASH/$dep" "$PACKAGE_SOURCE/@keymanapp"
  done

  rm -r "$PACKAGE_STASH"
}

# Parse args
# If no argument was provided, print help.
if [ $# -eq 0 ]; then
  display_usage
else
  case $1 in
    list-links)
      get_local_dependencies

      for dep in "${packages[@]}"; do
        echo "$dep"
      done
      ;;
    link-stash)
      QUIET=false
      stash_package_links
      ;;
    link-unstash)
      QUIET=false
      unstash_package_links
      ;;
    clean-external-deps)
      QUIET=true

      stash_package_links
      rm -rf node_modules
      unstash_package_links
      ;;
    # Default case - display help
    *)
      display_usage
      ;;
  esac
fi