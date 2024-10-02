#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

targets=()
# Build list of available targets from subfolders, if none specified
for d in */; do
  d="$(basename "$d")"
  if [ "$d" != "invalid" ] && [ "$d" != "issue" ]; then
    targets+=(":$d")
  fi
done

KMC="$KEYMAN_ROOT/developer/src/kmc/build/src/kmc.js"

builder_describe "Test Keyboards" \
  clean configure build \
  ${targets[@]} \
  "--index       Build index.html for artifact tests" \
  "--zip-source  Create zip file for source of each keyboard for artifact tests" \
  "--kmc=KMC     Specify path to kmc, defaults to developer/src/kmc/build/" \
  "--silent,-s   Suppress information messages"

builder_parse "$@"

function zipsource() {
  local target="$1"
  pushd "$1" > /dev/null
  7z a -r -x!build -x"!$target.kpj.user" "${target}_source.zip" .
  popd > /dev/null
}

function build_index() {
  local active_targets=($*)
  cat << EOF > index.html
<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>Test Keyboards - $VERSION_WITH_TAG</title>
  </head>
  <body>
    <h1>Test Keyboards - $VERSION_WITH_TAG</h1>
    <ul>
EOF

  for TARGET in "${active_targets[@]}"; do
    if builder_has_option --zip-source; then
      echo "      <li><a href='$TARGET/build/$TARGET.kmp'>$TARGET.kmp</a> (<a href='$TARGET/${TARGET}_source.zip'>source</a>)</li>" >> index.html
    else
      echo "      <li><a href='$TARGET/build/$TARGET.kmp'>$TARGET.kmp</a></li>" >> index.html
    fi
  done

  cat << EOF >> index.html
    </ul>
  </body>
</html>
EOF
}

###

function build() {
  local active_targets=()
  for TARGET in "${targets[@]}"; do
    if builder_has_action build$TARGET; then
      active_targets+=(${TARGET#:})
    fi
  done

  local ss=
  if builder_has_option --silent; then
    ss="--log-level silent"
  fi

  node "$KMC" build $builder_debug $ss -w "${active_targets[@]}"

  if builder_has_option --zip-source; then
    for TARGET in "${active_targets[@]}"; do
      zipsource "$TARGET"
    done
  fi

  if builder_has_option --index; then
    build_index "${active_targets[@]}"
  fi
}

builder_run_action build build