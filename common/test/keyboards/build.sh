#!/usr/bin/env bash
# TODO: builder script plz
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
THIS_DIR="$(dirname "$THIS_SCRIPT")"

display_usage() {
  echo "usage: build.sh [build options] [targets]"
  echo
  echo "Build options:"
  echo "  --debug, -d       Debug build"
  echo "  --silent, -s      Suppress information messages"
  echo "  --kmc path        Specify path to kmc, defaults"
  echo "                    to developer/src/kmc/build/"
  echo "  --zip-source      Create zip file for source of each"
  echo "                    keyboard for artifact tests"
  echo "  --index           Build index.html for artifact tests"
  echo
  echo "Targets (all unless specified):"

  for d in "$THIS_DIR/"*/; do
    d="$(basename "$d")"
    if [ "$d" != "invalid" && "$d" != "issue"]; then
      echo "  $d"
    fi
  done

  echo
  exit 0
}

readonly KMC_LAUNCHER=node
QUIET=false
DEBUG=false
INDEX=false
ZIPSOURCE=false
KMC="$KEYMAN_ROOT/developer/src/kmc/build/src/kmc.js"
TARGETS=()

# Parse args
shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    --help|-h|-\?)
      display_usage
      ;;
    --debug|-d)
      DEBUG=true
      ;;
    --silent|-s)
      QUIET=true
      ;;
    --zip-source)
      ZIPSOURCE=true
      ;;
    --index)
      INDEX=true
      ;;
    --kmc)
      shift
      KMC="$1"
      ;;
    *)
      TARGETS+=("$key")
  esac
  shift
done

# Build list of available targets from subfolders, if none specified
if [ ${#TARGETS[@]} == 0 ]; then
  for d in "$THIS_DIR/"*/; do
    d="$(basename "$d")"
    if [ "$d" != "invalid" ] && [ "$d" != "issue" ]; then
      TARGETS+=("$d")
    fi
  done
fi

if ! $QUIET; then
  displayInfo "" \
      "DEBUG: $DEBUG" \
      "QUIET: $QUIET" \
      "TARGETS: ${TARGETS[@]}" \
      "ZIPSOURCE: $ZIPSOURCE" \
      "INDEX: $INDEX" \
      ""
fi

zipsource() {
  local target="$1"
  pushd "$1" > /dev/null
  7z a -r -x!build -x"!$target.kpj.user" "${target}_source.zip" .
  popd > /dev/null
}

###

d=
ss=
if $DEBUG; then
  d=-d
fi
if $QUIET; then
  ss="--log-level silent"
fi
$KMC_LAUNCHER "$KMC" build $d $ss -w "${TARGETS[@]}"

for TARGET in "${TARGETS[@]}"; do
  if $ZIPSOURCE; then
    zipsource "$TARGET"
  fi
done

###

if $INDEX; then
  cat << EOF > "$THIS_DIR/index.html"
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

  for TARGET in "${TARGETS[@]}"; do
    if $ZIPSOURCE; then
      echo "      <li><a href='$TARGET/build/$TARGET.kmp'>$TARGET.kmp</a> (<a href='$TARGET/${TARGET}_source.zip'>source</a>)</li>" >> "$THIS_DIR/index.html"
    else
      echo "      <li><a href='$TARGET/build/$TARGET.kmp'>$TARGET.kmp</a></li>" >> "$THIS_DIR/index.html"
    fi
  done

  cat << 'EOF' >> "$THIS_DIR/index.html"
    </ul>
  </body>
</html>
EOF

fi
