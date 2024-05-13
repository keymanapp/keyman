# Define the generic build environment for Windows builds, including how to
# launch a Delphi build environment and a Visual Studio build environment

if ! _builder_has_function_been_called builder_parse; then
  builder_die "environment.inc.sh must be sourced after builder_parse is called (for debug flags)"
fi

source "$KEYMAN_ROOT/resources/build/win/configure_environment.inc.sh"

# We always build the generated enviroment files, even in a non-configure step,
# because e.g. clean may require visual studio environment to run
if [[ ! -f "$KEYMAN_ROOT/resources/build/win/environment_generated.inc.sh" ]] ||
  [[ ! -f "$KEYMAN_ROOT/resources/build/win/delphi_environment_generated.inc.sh" ]] ||
  [[ ! -f "$KEYMAN_ROOT/resources/build/win/visualstudio_environment_generated.inc.sh" ]]; then
  configure_windows_build_environment
fi

# We always source the global generated environment but not the compiler-specific generated environments
source "$KEYMAN_ROOT/resources/build/win/environment_generated.inc.sh"

# Windows common environment variables

WINDOWS_ROOT="$KEYMAN_ROOT/windows"
WINDOWS_PROGRAM="$WINDOWS_ROOT/bin"

COMMON_ROOT="$KEYMAN_ROOT/common/windows/delphi"
OUTLIB="$WINDOWS_ROOT/lib"
COMMON_OUTLIB="$KEYMAN_ROOT/common/windows/lib"

if builder_is_debug_build || [[ $VERSION_ENVIRONMENT == local ]] || [[ ! -z ${TEAMCITY_PR_NUMBER+x} ]]; then
  # We do a fast build for debug builds, local builds, test PR builds but not for master/beta/stable release builds
  GO_FAST=1
else
  GO_FAST=0
fi

# Import additional tool-specific variables

source "$KEYMAN_ROOT/resources/build/win/delphi_flags.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/vs_flags.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/signtime.inc.sh"

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
  replaceVersionStrings_Mkver manifest.in manifest.xml
  run_in_vs_env rc manifest.rc
}

clean_windows_project_files() {
  rm -rf bin obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

wrap-signcode() {
  # CI will usually pass in a full path for signtool.exe; for local builds we
  # will hopefully find what we want on the path already
  if [[ -z "${SIGNTOOL+x}" ]]; then
    local SIGNTOOL=signtool.exe
  fi
  signtime "$SIGNTOOL" "$SC_PFX_SHA1" "$SC_PFX_SHA256" "$SC_URL" "$SC_PWD" "$@"
}

wrap-symstore() {
  if [[  -z "${KEYMAN_SYMSTOREPATH+x}" ]]; then
    builder_warn "\$KEYMAN_SYMSTOREPATH is not set. Skipping symstore for $@"
    return 0
  fi

  "$ProgramFilesx86/Windows Kits/10/Debuggers/x64/symstore.exe" \
    add \
    //s "$KEYMAN_SYMSTOREPATH" \
    //v "$VERSION_WIN" \
    //c "Version: $VERSION_WITH_TAG" \
    //compress //f "$@"
}

wrap-mt() {
  run_in_vs_env mt.exe "$@"
}

create-developer-output-folders() {
  mkdir -p "$DEVELOPER_PROGRAM/xml"
  mkdir -p "$DEVELOPER_DEBUGPATH"
}