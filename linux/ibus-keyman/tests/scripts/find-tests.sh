#!/usr/bin/env bash
set -eu
while [ -n "$1" ]; do
  if [ ! -d "$1" ]; then
    shift
    continue
  fi
  pushd "$1" > /dev/null
  while IFS= read -r -d '' file; do
    testname=$(basename "$file" .kmx)
    #shellcheck disable=SC2059
    printf "$(basename "$file")\t${testname#k_}\n"
  done < <(find . -name \*.kmx -print0 | sort -z)
  popd > /dev/null
  exit 0
done
