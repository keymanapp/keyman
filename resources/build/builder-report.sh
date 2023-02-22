#!/usr/bin/env bash
#
# Reports on all builder scripts in repo and builds a dependency tree
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$KEYMAN_ROOT"

builder_scripts=(`find . -name "build.sh" | xargs grep -l "builder_describe"`)
non_builder_scripts=(`find . -name "build.sh" | xargs grep -L "builder_describe"`)

declare -A dependencies

function get_script_dependencies() {
  local script="${1#./}"
  if [[ $script =~ "node_modules/" ]]; then
    # we may get builder scripts copied into node_modules by npm
    return 0
  fi

  script="${script%/build.sh}"
  echo "$script"
  local deps=`./$script/build.sh --builder-report-dependencies`
  dependencies[$script]="$deps"
}

echo
echo "${COLOR_BLUE}## Locating builds${COLOR_RESET}"
echo

for e in "${builder_scripts[@]}"; do
  get_script_dependencies "$e"
done

# Find top-level projects

root_dependencies=()
for e in "${!dependencies[@]}"; do
  if [[ ! " ${dependencies[@]} " =~ " $e " ]]; then
    root_dependencies+=($e)
  fi
done

# Sort results

IFS=$'\n' root_dependencies=($(sort <<<"${root_dependencies[*]}")); unset IFS
IFS=$'\n' sorted_dependencies=($(sort <<<"${!dependencies[*]}")); unset IFS

# Print project dependency tree

function print_dependencies() {
  local depth=$1
  local name=$2
  if (($depth > 0 )); then printf '  %.0s' $(seq 1 $depth); fi
  echo "$name"
  for e in ${dependencies[$name]}; do
    if [[ -z $e ]]; then continue; fi
    print_dependencies $((depth + 1)) $e
  done
}

echo
echo "${COLOR_BLUE}## Project Dependency Tree${COLOR_RESET}"
echo
for e in "${root_dependencies[@]}"; do
  print_dependencies 0 $e
  echo
done

# Print list of all non-builder scripts

echo "${COLOR_BLUE}## Legacy build scripts (not using builder)${COLOR_RESET}"
echo
for e in "${non_builder_scripts[@]}"; do
  scriptname="${e#./}"
  if [[ $scriptname =~ "node_modules/" ]]; then
    # we may get build.sh scripts copied into node_modules by npm
    continue
  fi

  scriptname="${scriptname%/build.sh}"
  echo "$scriptname"
done

# Print list of all projects and dependencies

echo
echo "${COLOR_BLUE}## All Projects and Dependencies${COLOR_RESET}"
echo
for e in "${sorted_dependencies[@]}"; do
  echo "$e: ${dependencies[$e]}"
done
echo
