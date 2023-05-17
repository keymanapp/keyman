#!/usr/bin/env bash

function can_run_wayland() {
  local MUTTER_VERSION
  MUTTER_VERSION=$(mutter --version | head -1 | cut -f2 -d' ' | cut -f1 -d'.')
  if (( MUTTER_VERSION < 40 )); then
    return 1
  else
    return 0
  fi
}

function generate_kmpjson() {
  local TESTDIR
  TESTDIR="$1"
  pushd "$TESTDIR" > /dev/null || exit
  KMPFILE="${TESTDIR}/kmp.json"
  cat <<-EOF > "$KMPFILE"
  {
    "system": {
      "keymanDeveloperVersion": "10.0.1099.0",
      "fileVersion": "7.0"
    },
    "info": {
      "name": {
        "description": "Test Keyboards"
      },
      "version": {
        "description": "0.0"
      }
    },
    "files": [
      {
        "name": "kmp.json",
        "description": "Package information (JSON)"
      }
EOF
  for f in k_*.kmx; do
    keyboard=$(basename "$f")
    keyboard="${keyboard%.*}"
    echo "      ," >> "$KMPFILE"
    cat <<-EOF >> "$KMPFILE"
      {
        "name": "$keyboard",
        "description": "$keyboard"
      }
EOF
  done
  cat <<-EOF >> "$KMPFILE"
    ],
    "keyboards": [
EOF
  FIRST=true
  for f in k_*.kmx; do
    keyboard=$(basename "$f")
    keyboard="${keyboard%.*}"
    if $FIRST; then
      FIRST=false
    else
      echo "    ," >> "$KMPFILE"
    fi
    cat <<-EOF >> "$KMPFILE"
    {
      "name": "$keyboard",
      "id": "$keyboard",
      "version": "0.0",
      "languages": [
        {
          "name": "Undetermined",
          "id": "und"
        }
      ]
    }
EOF
  done
  echo "  ]" >> "$KMPFILE"
  echo "}" >> "$KMPFILE"
  popd > /dev/null || exit
}

function link_test_keyboards() {
  KMX_TEST_DIR=$1
  TESTDIR=$2
  TESTBASEDIR=$3

  if [[ $(find "${KMX_TEST_DIR}/" -name k_\*.kmx 2>/dev/null | wc -l) -gt 0 ]]; then
    mkdir -p "$(realpath --canonicalize-missing "$TESTBASEDIR")"
    rm -f "$TESTDIR"
    ln -sf "$(realpath "${KMX_TEST_DIR}")" "$TESTDIR"
  else
    echo "Can't find test kmx files in ${KMX_TEST_DIR}"
    exit 3
  fi
}
