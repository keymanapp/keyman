# Define the generic build environment for Windows builds, including how to
# launch a Delphi build environment and a Visual Studio build environment

source "$KEYMAN_ROOT/resources/build/win/configure_environment.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/delphi_flags.inc.sh"

run_in_vs_env() {
  (
    builder_echo heading "### visual_studio: $@"
    source "$KEYMAN_ROOT/resources/build/win/visualstudio_environment.inc.sh"
    "$@"
  )
}

run_in_delphi_env() {
  (
    builder_echo heading "### delphi: $@"
    source "$KEYMAN_ROOT/resources/build/win/delphi_environment.inc.sh"
    "$@"
  )
}

sentrytool_delphiprep() {
  local EXE_PATH="$1"
  local DPR_PATH="$2"
  (
    builder_echo heading "### delphi: sentrytool_delphiprep $1 $2"
    source "$KEYMAN_ROOT/resources/build/win/delphi_environment.inc.sh"
    "$SENTRYTOOL" delphiprep -r "$KEYMAN_ROOT" -i "$DELPHIINCLUDES" "$(cygpath -w "$EXE_PATH")" -dpr "$DPR_PATH"
  )
}

tds2dbg() {
  "$TDS2DBG" "$@"
}

delphi_msbuild() {
  run_in_delphi_env msbuild.exe "$@" "$DELPHI_MSBUILD_FLAG_DEBUG"
}

build_version.res() {
  run_in_vs_env rc version.rc
}

build_manifest.res() {
  "$KEYMAN_ROOT/common/windows/mkver.sh" manifest.in manifest.xml
  run_in_vs_env rc manifest.rc
}
