#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../..../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

source "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Installation files for FirstVoices Keyboards" \
  @/common/windows/data \
  clean configure build test publish

# NOTE: not using deps here because we will only do this in the 'publish' phase
# after all other builds complete

builder_describe_outputs \
  publish       /oem/firstvoices/windows/src/inst/firstvoices-${VERSION}.exe

builder_parse "$@"

. "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/wix.inc.sh"
. "$KEYMAN_ROOT/resources/build/win/zip.inc.sh"

# In dev environments, we'll hack the tier to alpha; CI sets this for us in real builds.
if [[ -z ${TIER+x} ]]; then
  TIER=alpha
fi

# TODO: why are we not bundling CEF?
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

function do_publish() {
  replaceVersionStrings_Mkver ../xml/sentry.init.js.in ../xml/sentry.init.js

  #
  # Get latest FirstVoices keyboards fv_all.kmp
  #
  downloadKeyboardPackage fv_all fv_all.kmp

  #
  # Build the installation archive
  #

  local GUID1=$(generate_uuid)

  "$WIXHEAT" dir ../xml -o desktopui.wxs -ag -cg DesktopUI -dr INSTALLDIR -suid -var var.DESKTOPUISOURCE -wx -nologo
  "$WIXCANDLE" -dOEMNAME="FirstVoices" -dPRODUCTNAME="FirstVoices Keyboards" -dROOT="$KEYMAN_ROOT/windows" \
     -dVERSION=$VERSION_WIN -dRELEASE=$VERSION_RELEASE \
     -dPRODUCTID=$GUID1 -dDESKTOPUISOURCE=../xml \
     firstvoices.wxs desktopui.wxs
  "$WIXLIGHT" -dWixUILicenseRtf=License.rtf -out firstvoices.msi -ext WixUIExtension firstvoices.wixobj desktopui.wixobj

  #
  # Sign the installation archive
  #
  wrap-signcode //d "FirstVoices Keyboards" firstvoices.msi

  #
  # Build self-extracting archive
  #
  create-setup-inf
  wzzip firstvoices.zip firstvoices.msi license.html setup.inf setuptitle.png fv_all.kmp
  rm -f setup.inf
  cat "$WINDOWS_PROGRAM_APP/setup-redist.exe" firstvoices.zip > firstvoices.exe
  rm -f firstvoices.zip

  #
  # Sign the installer
  #
  wrap-signcode //d "FirstVoices Keyboards" firstvoices.exe

  copy-installer
}

function copy-installer() {
  builder_heading copy-installer

  mkdir -p "$KEYMAN_ROOT/windows/release/${VERSION}"
  cp firstvoices.msi "$KEYMAN_ROOT/windows/release/${VERSION}/firstvoices.msi"
  cp firstvoices.exe "$KEYMAN_ROOT/windows/release/${VERSION}/firstvoices-${VERSION}.exe"

  verify-installer-signatures
}

function verify-installer-signatures() {
  builder_heading verify-installer-signatures

  verify-all-executable-signatures-in-folder "$KEYMAN_ROOT/windows/release/${VERSION}"
}

function create-setup-inf() {
  builder_heading create-setup-inf

  echo "[Setup]" > setup.inf
  echo "Version=$VersionWin" >> setup.inf
  echo "MSIFileName=firstvoices.msi" >> setup.inf
  echo "MSIOptions=" >> setup.inf
  echo "AppName=FirstVoices Keyboards" >> setup.inf
  echo "License=license.html" >> setup.inf
  echo "TitleImage=setuptitle.png" >> setup.inf
  echo "StartDisabled=True" >> setup.inf
  echo "StartWithConfiguration=True" >> setup.inf
  echo "[Packages]" >> setup.inf
  echo "fv_all.kmp=Language Keyboard Packages" >> setup.inf
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean     do_clean
# builder_run_action configure do_configure
# builder_run_action build     do_build
# builder_run_action test      do_test
builder_run_action publish   do_publish
