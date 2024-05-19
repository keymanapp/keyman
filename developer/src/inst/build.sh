#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

source "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Installation files for Keyman Developer" \
  @/common/windows/data \
  clean configure build test publish

# NOTE: not using deps here because we will only do this in the 'publish' phase
# after all other builds complete

builder_describe_outputs \
  publish       /developer/src/inst/keymandeveloper-${VERSION}.exe

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
KEYMAN_WIX_TEMP_XML="$TEMP/keyman_wix_build/xml"
KEYMAN_WIX_TEMP_CEF="$TEMP/keyman_wix_build/cef"
KEYMAN_WIX_TEMP_TEMPLATES="$TEMP/keyman_wix_build/templates"
KEYMAN_WIX_TEMP_KMC="$TEMP/keyman_wix_build/kmc"
KEYMAN_WIX_TEMP_SERVER="$TEMP/keyman_wix_build/Server"

KEYMAN_WIX_KMDEV_SERVER="$DEVELOPER_ROOT/bin/server"
KEYMAN_DEVELOPER_TEMPLATES_ROOT="$DEVELOPER_ROOT/src/kmconvert/data"

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -f *.msi *.wixobj *.log setup.inf setup.zip *.exe *.wixpdb xml.wxs cef.wxs templates.wxs kmc.wxs
  rm -rf "$DEVELOPER_ROOT/src/tike/xml/osk"
}

#-------------------------------------------------------------------------------------------------------------------

function do_publish() {
  verify-program-signatures

  "$KEYMAN_ROOT/common/windows/cef-checkout.sh"

  #
  # Build the installation archive
  #
  candle

  "$WIXLIGHT" \
    -wx -nologo "${WIXLIGHTLINT[@]}" "$WIXLIGHTCOMPRESSION" \
    -sice:ICE91 -sice:ICE60 -dWixUILicenseRtf=License.rtf \
    -out keymandeveloper.msi -ext WixUIExtension \
    kmdev.wixobj xml.wixobj cef.wixobj templates.wixobj server.wixobj kmc.wixobj

  clean-heat-kmc

  #
  # Sign the installation archive
  #
  wrap-signcode //d "Keyman Developer" keymandeveloper.msi

  #
  # Copy the installation archive
  #

  copy-kmdev

  verify-installer-signatures
}

function copy-kmdev() {
  builder_heading copy-kmdev

  mkdir -p "$DEVELOPER_ROOT/release/${VERSION}"

  make-installer
  make-kmc-install-zip

  cp -f "$DEVELOPER_ROOT/src/inst/keymandeveloper.msi" "$DEVELOPER_ROOT/release/${VERSION}/keymandeveloper.msi"
  cp -f "$DEVELOPER_ROOT/src/inst/keymandeveloper-${VERSION}.exe" "$DEVELOPER_ROOT/release/${VERSION}/keymandeveloper-${VERSION}.exe"
}

function verify-program-signatures() {
  builder_heading verify-program-signatures

  verify-all-executable-signatures-in-folder "$DEVELOPER_ROOT/bin"
}

function verify-installer-signatures() {
  builder_heading verify-installer-signatures

  verify-all-executable-signatures-in-folder "$DEVELOPER_ROOT/release/${VERSION}"
}

function test-releaseexists() {
  if [[ -d "$DEVELOPER_ROOT/release/${VERSION}" ]]; then
    builder_die "Release ${VERSION} already exists. Delete it or update VERSION.md and try again"
  fi
}

function candle() {
  builder_heading candle

  heat-cef
  heat-xml
  heat-templates
  heat-server
  heat-kmc

  builder_heading "candle: calling WIX:candle"
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" kmdev.wxs
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" "-dXmlSourceDir=${DEVELOPER_ROOT}/src/tike/xml" xml.wxs
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" "-dCefSourceDir=${KEYMAN_CEF4DELPHI_ROOT}" cef.wxs
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" "-dTemplatesSourceDir=${KEYMAN_DEVELOPER_TEMPLATES_ROOT}" templates.wxs
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" "-dkmcSourceDir=${KEYMAN_WIX_TEMP_KMC}" kmc.wxs
  "$WIXCANDLE" -wx -nologo "-dVERSION=${VERSION_WIN}" "-dRELEASE=${VERSION_RELEASE}" "-dServerSourceDir=${KEYMAN_WIX_KMDEV_SERVER}" server.wxs
}

function heat-xml() {
  builder_heading heat-xml

  # We copy the files to a temp folder in order to exclude thumbs.db, .vs, etc from harvesting
  # We also copy over the OSK files from Keyman Engine (#11199)
  rm -rf "$KEYMAN_WIX_TEMP_XML"
  mkdir -p "$KEYMAN_WIX_TEMP_XML"
  cp -r "$KEYMAN_ROOT/windows/src/engine/xml/osk" "$DEVELOPER_ROOT/src/tike/xml/osk"
  cp -r "$DEVELOPER_ROOT/src/tike/xml"/* "$KEYMAN_WIX_TEMP_XML"

  rm -f "$KEYMAN_WIX_TEMP_XML/Thumbs.db"
  rm -rf "$KEYMAN_WIX_TEMP_XML/app/node_modules"
  find "$KEYMAN_WIX_TEMP_XML" -name '.vs' -print0 | xargs -0 rm -rf
  "$WIXHEAT" dir "$KEYMAN_WIX_TEMP_XML" -o xml.wxs -ag -cg XML -dr INSTALLDIR -var var.XmlSourceDir -wx -nologo

  # When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
  rm -rf "$KEYMAN_WIX_TEMP_XML"
}

function heat-templates() {
  builder_heading heat-templates

  # We copy the files to a temp folder in order to exclude .git and README.md from harvesting
  rm -rf "$KEYMAN_WIX_TEMP_TEMPLATES"
  mkdir -p "$KEYMAN_WIX_TEMP_TEMPLATES"
  cp -r "$KEYMAN_DEVELOPER_TEMPLATES_ROOT"/* "$KEYMAN_WIX_TEMP_TEMPLATES"
  "$WIXHEAT" dir "$KEYMAN_WIX_TEMP_TEMPLATES" -o templates.wxs -ag -cg Templates -dr dirProjects -var var.TemplatesSourceDir -wx -nologo
  # When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
  rm -rf "$KEYMAN_WIX_TEMP_TEMPLATES"
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

function heat-server() {
  builder_heading heat-server

  # We copy the files to a temp folder in order to exclude .git and README.md from harvesting
  rm -rf "$KEYMAN_WIX_TEMP_SERVER"
  mkdir -p "$KEYMAN_WIX_TEMP_SERVER"
  cp -r "$KEYMAN_WIX_KMDEV_SERVER"/* "$KEYMAN_WIX_TEMP_SERVER"
  "$WIXHEAT" dir "$KEYMAN_WIX_TEMP_SERVER" -o server.wxs -ag -cg Server -dr INSTALLDIR -var var.ServerSourceDir -wx -nologo
  # When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
  rm -rf "$KEYMAN_WIX_TEMP_SERVER"
}

function heat-kmc() {
  builder_heading heat-kmc

  cd "$DEVELOPER_ROOT/src/kmc"
  # Build the distributable package
  mkdir -p "$KEYMAN_WIX_TEMP_KMC"
  # TODO: BUNDLE CORRECTLY
  ./build.sh bundle --build-path "$KEYMAN_WIX_TEMP_KMC"

  # Build the .wxs file
  cd "$THIS_SCRIPT_PATH"
  "$WIXHEAT" dir "$KEYMAN_WIX_TEMP_KMC" -o kmc.wxs -ag -cg kmc -dr INSTALLDIR -var var.kmcSourceDir -wx -nologo
}

function clean-heat-kmc() {
  builder_heading clean-heat-kmc

  # the production build generates files that are not in source, e.g. .ps1 scripts
  # When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
  rm -rf "$KEYMAN_WIX_TEMP_KMC"
}

function make-installer() {
  builder_heading make-installer

  echo [Setup] > setup.inf
  echo "Version=$VERSION" >> setup.inf
  echo "MSIFileName=keymandeveloper.msi" >> setup.inf
  echo "Title=Keyman Developer ${VERSION_WITH_TAG}" >>setup.inf
  wzzip setup.zip keymandeveloper.msi setup.inf
  cat "$DEVELOPER_PROGRAM/setup.exe" setup.zip > "keymandeveloper-$VERSION.exe"
  wrap-signcode //d "Keyman Developer" "keymandeveloper-$VERSION.exe"

  #
  # Zip the files we distribute as part of the standalone kmc distro into release\$Version\kmcomp-$Version.zip
  #
}

# TODO: rename this to keyman-developer-cli-$Version.zip
KMC_ZIP="$DEVELOPER_ROOT/release/$VERSION/kmcomp-$VERSION.zip"

function make-kmc-install-zip() {
  builder_heading make-kmc-install-zip

  copy-schemas
  cd "$DEVELOPER_ROOT/bin"

  wzzip -bd -bb0 "$KMC_ZIP" \
    kmconvert.exe \
    sentry.dll sentry.x64.dll \
    kmdecomp.exe \
    keyboard_info.schema.json \
    kmp.schema.json \
    keyman-touch-layout.spec.json keyman-touch-layout.clean.spec.json \
    xml/layoutbuilder/*.keyman-touch-layout \
    projects/* \
    server/*

  cd "$THIS_SCRIPT_PATH"
}

# TODO: are these required?
# kpj.schema.json kvks.schema.json \
# ldml-keyboard3.schema.json ldml-keyboardtest3.schema.json \

function copy-schemas() {
  builder_heading copy-schemas

  cp "$KEYMAN_ROOT/common/schemas/keyboard_info/keyboard_info.schema.json" "$DEVELOPER_ROOT/bin"
  cp "$KEYMAN_ROOT/common/schemas/keyman-touch-layout/keyman-touch-layout.spec.json" "$DEVELOPER_ROOT/bin"
  cp "$KEYMAN_ROOT/common/schemas/keyman-touch-layout/keyman-touch-layout.clean.spec.json" "$DEVELOPER_ROOT/bin"
  cp "$KEYMAN_ROOT/common/schemas/displaymap/displaymap.schema.json" "$DEVELOPER_ROOT/bin"
  cp "$KEYMAN_ROOT/common/schemas/kmp/kmp.schema.json" "$DEVELOPER_ROOT/bin"
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean     do_clean
# builder_run_action configure do_configure
# builder_run_action build     do_build
# builder_run_action test      do_test
builder_run_action publish   do_publish
