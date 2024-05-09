# Define the generic build environment for Windows builds, including how to
# launch a Delphi build environment and a Visual Studio build environment

if ! _builder_has_function_been_called builder_parse; then
  builder_die "environment.inc.sh must be sourced after builder_parse is called"
fi

source "$KEYMAN_ROOT/resources/build/win/configure_environment.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/delphi_flags.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/vs_flags.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/signtime.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/zip.inc.sh"

VERIFY_SIGNATURES_PATH="$KEYMAN_ROOT/common/windows/delphi/tools/verify_signatures"
VERIFY_SIGNATURES="$VERIFY_SIGNATURES_PATH/$WIN32_TARGET_PATH/verify_signatures.exe"
SIGCHECK="$VERIFY_SIGNATURES_PATH/$WIN32_TARGET_PATH/sigcheck.exe"

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

vs_msbuild() {
  run_in_vs_env msbuild.exe "$@" "$VS_MSBUILD_FLAG_DEBUG"
}

build_version.res() {
  run_in_vs_env rc version.rc
}

build_manifest.res() {
  "$KEYMAN_ROOT/common/windows/mkver.sh" manifest.in manifest.xml
  run_in_vs_env rc manifest.rc
}

clean_windows_project_files() {
  rm -rf bin obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

wrap-signcode() {
  signtime signtool.exe "$SC_PFX_SHA1" "$SC_PFX_SHA256" "$SC_URL" "$SC_PWD" "$@"
}

wrap-symstore() {
  if [[  -z "${KEYMAN_SYMSTOREPATH+x}" ]]; then
    builder_warn "\$KEYMAN_SYMSTOREPATH is not set. Skipping symstore for $@"
    return 0
  fi

  "/c/Program Files \(x86\)/Windows Kits/10/Debuggers/x64/symstore.exe" \
    add \
    //s "$KEYMAN_SYMSTOREPATH" \
    //v "$VERSION_WIN" \
    //c "Version: $VERSION_WITH_TAG" \
    //compress //f "$@"
}

wrap-mt() {
  run_in_vs_env mt.exe "$@"
}