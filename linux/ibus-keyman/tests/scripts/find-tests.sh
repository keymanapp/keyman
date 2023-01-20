#!/usr/bin/env bash
cd "$1" || exit
while IFS= read -r -d '' file; do
  testname=$(basename "$file" .kmx)
  printf "$(basename "$file")\t${testname#k_}\n"
done < <(find . -name \*.kmx -print0 | sort -z)
