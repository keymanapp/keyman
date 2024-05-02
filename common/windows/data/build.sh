#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Build shared data" \
  clean configure build test \
  "@../delphi/tools/buildunidata"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/bin/data/unicodedata.mdb

#-------------------------------------------------------------------------------------------------------------------

BUILDUNIDATA_PATH="$KEYMAN_ROOT/common/windows/delphi/tools/buildunidata"
BUILDUNIDATA_PROGRAM="$BUILDUNIDATA_PATH/$WIN32_TARGET_PATH/buildunidata.exe"
DATA_SOURCE="$KEYMAN_ROOT/resources/standards-data/unicode-character-database"
DATABASE_PATH="$KEYMAN_ROOT/common/windows/bin/data"
DATABASE_FILENAME="$DATABASE_PATH/unicodedata.mdb"

function do_clean() {
  rm -f "$DATABASE_FILENAME"
}

function do_build() {
  mkdir -p "$DATABASE_PATH"
  # For now, we need to use cygpath for these parameters due to OLE
  # incompatibilities with forward-slash in paths.
  "$BUILDUNIDATA_PROGRAM" "$(cygpath -w "$DATA_SOURCE")" "$(cygpath -w "$DATABASE_FILENAME")"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
