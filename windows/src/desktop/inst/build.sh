#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

source "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Installation files for Keyman for Windows" \
  @/common/windows/data \
  clean configure build test publish

# NOTE: not using deps here because we will only do this in the 'publish' phase
# after all other builds complete

builder_describe_outputs \
  publish       /windows/release/${VERSION}/keyman-${VERSION}.exe

builder_parse "$@"

. "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/wix.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/zip.inc.sh"

# In dev environments, we'll hack the tier to alpha; CI sets this for us in real builds.
if [[ -z ${TIER+x} ]]; then
  TIER=alpha
fi

# We use different directories so that heat generates
# different identifiers for the various folders
KEYMAN_WIX_TEMP_BASE="$TEMP/keyman_wix_build"
KEYMAN_WIX_TEMP_CEF="$TEMP/keyman_wix_build/cef"

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -f *.msi *.wixobj *.log setup.inf setup.zip keymandesktop.zip *.exe *.wixpdb \
    desktop_resources.dll desktop_resources.res \
    cef.wxs locale.wxs desktopui.wxs
}

#-------------------------------------------------------------------------------------------------------------------

function do_build_desktop_resources() {
  #
  # Create resource dll
  #
  run_in_vs_env rc desktop_resources.rc
  run_in_vs_env link //DLL //OUT:desktop_resources.dll //RELEASE //NOENTRY //MACHINE:X86 desktop_resources.RES
  wrap-signcode //d "Keyman Resources" desktop_resources.dll
}

function do_publish() {
  verify-program-signatures

  "$KEYMAN_ROOT/common/windows/cef-checkout.sh"

  do_build_desktop_resources

  #
  # Build the installation archive
  #
  do_candle

  # ICE82: we suppress because it reports spurious errors with merge module
  #        keymanengine to do with duplicate sequence numbers.  Safely ignored.
  # ICE80: we suppress because it reports x64 components without targeting x64.
  #        Safely ignored.

  "$WIXLIGHT" \
    -sice:ICE82 -sice:ICE80 \
    -nologo \
    -dWixUILicenseRtf=License.rtf \
    -out keymandesktop.msi -ext WixUIExtension \
    keymandesktop.wixobj desktopui.wixobj cef.wixobj locale.wixobj

  #
  # Sign the installation archive
  #
  wrap-signcode //d "Keyman for Windows" keymandesktop.msi

  #
  # Build self-extracting archive
  #
  create-setup-inf
  wzzip keymandesktop.zip keymandesktop.msi license.html setup.inf
  rm -f setup.inf
  cat "$WINDOWS_PROGRAM_APP/setup-redist.exe" keymandesktop.zip > keymandesktop.exe
  rm -f keymandesktop.zip

  #
  # Sign the installer
  #
  wrap-signcode //d "Keyman for Windows" keymandesktop.exe

  copy-installer
}

function copy-installer() {
  builder_heading copy-installer

  mkdir -p "$KEYMAN_ROOT/windows/release/${VERSION}"
  cp keymandesktop.msi "$KEYMAN_ROOT/windows/release/${VERSION}/keymandesktop.msi"
  cp keymandesktop.exe "$KEYMAN_ROOT/windows/release/${VERSION}/keyman-${VERSION}.exe"
  cp "$WINDOWS_PROGRAM_APP/setup.exe" "$KEYMAN_ROOT/windows/release/${VERSION}/setup.exe"

  verify-installer-signatures

  # Copy the unsigned setup.exe for use in bundling scenarios; zip it up for clarity
  wzzip "$KEYMAN_ROOT/windows/release/${VERSION}/setup-redist.zip" "$WINDOWS_PROGRAM_APP/setup-redist.exe"
}

function verify-program-signatures() {
  builder_heading verify-program-signatures

  verify-all-executable-signatures-in-folder "$KEYMAN_ROOT/windows/bin/desktop"
}

function verify-installer-signatures() {
  builder_heading verify-installer-signatures

  verify-all-executable-signatures-in-folder "$KEYMAN_ROOT/windows/release/${VERSION}"
}

function test-releaseexists() {
  if [[ -d "$KEYMAN_ROOT/windows/release/${VERSION}" ]]; then
    builder_die "Release ${VERSION} already exists. Delete it or update VERSION.md and try again"
  fi
}

function do_candle() {
  heat-cef

  builder_heading candle

  local GUID1=$(generate_uuid)
  "$WIXHEAT" dir ../kmshell/xml -o desktopui.wxs -ag -cg DesktopUI -dr INSTALLDIR -suid -var var.DESKTOPUISOURCE -wx -nologo
  "$WIXHEAT" dir ../kmshell/locale -o locale.wxs -ag -cg Locale -dr INSTALLDIR -var var.LOCALESOURCE -wx -nologo
  "$WIXCANDLE" -dVERSION_WITH_TAG=${VERSION_WITH_TAG} -dVERSION=${VERSION_WIN} -dRELEASE=${VERSION_RELEASE} -dPRODUCTID=$GUID1 \
    -dDESKTOPUISOURCE=../kmshell/xml -dLOCALESOURCE=../kmshell/locale "-dCefSourceDir=$KEYMAN_CEF4DELPHI_ROOT" \
    keymandesktop.wxs desktopui.wxs locale.wxs cef.wxs
}

function heat-cef() {
  builder_heading heat-cef

  # We copy the files to a temp folder in order to exclude .git and README.md from harvesting
  rm -rf "$KEYMAN_WIX_TEMP_CEF"
  mkdir -p "$KEYMAN_WIX_TEMP_CEF"
  cp -r "$KEYMAN_CEF4DELPHI_ROOT"/* "$KEYMAN_WIX_TEMP_CEF"
  "$WIXHEAT" dir "$KEYMAN_WIX_TEMP_CEF" -o cef.wxs -ag -cg CEF -dr INSTALLDIR -var var.CefSourceDir -wx -nologo
  # When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
  rm -rf "$KEYMAN_WIX_TEMP_CEF"
}


function create-setup-inf() {
  builder_heading create-setup-inf

  echo "[Setup]" > setup.inf
  echo "Version=${VERSION_WIN}" >> setup.inf
  echo "MSIFileName=keymandesktop.msi" >> setup.inf
  echo "MSIOptions=" >> setup.inf
  echo "License=license.html" >> setup.inf
  echo "[Packages]" >> setup.inf
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean     do_clean
# builder_run_action configure do_configure
# builder_run_action build     do_build
# builder_run_action test      do_test
builder_run_action publish   do_publish
