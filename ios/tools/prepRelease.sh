#!/usr/bin/env bash

## Note - this script is designed solely for use within our Continuous
## Integration processes (thus, by ../ci.sh) with `pwd` as this script's parent folder.

# Exit if program call fails
set -e

# Exit if unset variable used
## set -u (causes history-utils.sh to fail)

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its parent's folder
cd "$(dirname "$THIS_SCRIPT")/.."

# Only allow one upload artifact set to exist.
if [ -d "upload" ]; then
  rm -rf upload
fi

#Include script dependency
. $KEYMAN_ROOT/resources/build/history/history-utils.sh         #includes the following
#. ../resources/shellHelperFunctions.sh

BUILD_NUMBER=`cat ../VERSION.md`
TIER=`cat ../TIER.md`

KEYMAN_CHANGELOG="changelog-${BUILD_NUMBER}-${TIER}.txt"
CHANGELOG_PATH="upload/$BUILD_NUMBER/$KEYMAN_CHANGELOG"

WORK_DIR=`pwd`

BUILD_COUNTER="$((${BUILD_NUMBER##*.}))"

UPLOAD_BASE="upload"
UPLOAD_FOLDER="${BUILD_NUMBER}"
UPLOAD_DIR="${UPLOAD_BASE}/${UPLOAD_FOLDER}"

mkdir -p "${UPLOAD_DIR}"

# First, we prep the files for publication: write changelog

echo "Writing changelog to $CHANGELOG_PATH"
get_version_notes "ios" "${BUILD_NUMBER}" "$TIER" > $CHANGELOG_PATH
echo "* Minor fixes and performance improvements" >> $CHANGELOG_PATH
assertFileExists "${CHANGELOG_PATH}"

# Strip emoji as App Store does not allow emoji in changelogs
node "$KEYMAN_ROOT/resources/tools/strip-emoji" < "$CHANGELOG_PATH" > "$CHANGELOG_PATH.1"
mv -f "$CHANGELOG_PATH.1" "$CHANGELOG_PATH"
assertFileExists "${CHANGELOG_PATH}"

#
# Keyman Engine
#

KMEI_DST_NAME="keyman-engine-ios-${BUILD_NUMBER}.zip"
KMEI_DST="${WORK_DIR}/${UPLOAD_DIR}/${KMEI_DST_NAME}"

KMEI_FRAMEWORK_BASE="build/Build/Products/Release/"
FRAMEWORK="KeymanEngine.xcframework"

KEYMAN_SAMPLES="samples"

echo "engine dest: $KMEI_DST"

echo "Zipping ${FRAMEWORK} => ${UPLOAD_DIR}/${KMEI_DST}..."
cd "${KMEI_FRAMEWORK_BASE}"
zip -qrX "${KMEI_DST}" ${FRAMEWORK}
cd "$WORK_DIR"

echo "Copying Keyman Engine samples into ${UPLOAD_DIR}/${KMEI_DST_NAME}..."
cp -rf "${KEYMAN_SAMPLES}" "${UPLOAD_DIR}/samples"
cd "${UPLOAD_DIR}"
zip -qr "${KMEI_DST_NAME}" "samples"
rm -rf "samples"
cd "$WORK_DIR"

#
# Keyman app
#

KEYMANAPP_IPA="build/Build/Products/Release-iphoneos/Keyman.ipa"
KEYMANAPP_IPA_DST="keyman-ios-${BUILD_NUMBER}.ipa"

echo "Copying Keyman IPA ${KEYMANAPP_IPA} => ${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}..."
cp "${KEYMANAPP_IPA}" "${WORK_DIR}/${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}"

KEYMANAPP_SIM_FOLDER="build/Build/Products/Release-iphonesimulator"
KEYMANAPP_SIM_APP="$KEYMANAPP_SIM_FOLDER/Keyman.app"
KEYMANAPP_SIM_APP_DST="keyman-ios-simulator-${BUILD_NUMBER}.app.zip"

echo "Zipping Keyman simulator artifact ${KEYMANAPP_SIM_APP} => ${UPLOAD_DIR}/${KEYMANAPP_SIM_APP_DST}..."
cd "${KEYMANAPP_SIM_FOLDER}"
zip -qrX "${WORK_DIR}/${UPLOAD_DIR}/${KEYMANAPP_SIM_APP_DST}" "Keyman.app"
echo "${WORK_DIR}/${UPLOAD_DIR}/${KEYMANAPP_SIM_APP_DST}"
cd "$WORK_DIR"

#
# FirstVoices app
#

if [ "${RELEASE_OEM_FIRSTVOICES}" = true ]; then
  FIRSTVOICESAPP_IPA="../oem/firstvoices/ios/build/Build/Products/Release-iphoneos/FirstVoices.ipa"
  FIRSTVOICESAPP_IPA_DST="firstvoices-ios-${BUILD_NUMBER}.ipa"

  echo "Copying FirstVoices IPA ${FIRSTVOICESAPP_IPA} => ${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}..."
  cp "${FIRSTVOICESAPP_IPA}" "${WORK_DIR}/${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}"

  FIRSTVOICESAPP_SIM_FOLDER="../oem/firstvoices/ios/build/Build/Products/Release-iphonesimulator/"
  FIRSTVOICESAPP_SIM_APP="$FIRSTVOICESAPP_SIM_FOLDER/FirstVoices.app"
  FIRSTVOICESAPP_SIM_APP_DST="firstvoices-ios-simulator-${BUILD_NUMBER}.app.zip"

  echo "Zipping FirstVoices simulator artifact ${FIRSTVOICESAPP_SIM_APP} => ${UPLOAD_DIR}/${KEYMANAPP_SIM_APP_DST}..."
  cd "${FIRSTVOICESAPP_SIM_FOLDER}"
  zip -qrX "${WORK_DIR}/${UPLOAD_DIR}/${FIRSTVOICESAPP_SIM_APP_DST}" "FirstVoices.app"
  cd "$WORK_DIR"
fi

#
# Write download info files
#

cd "${UPLOAD_DIR}"

write_download_info "Keyman Engine for iOS" "${KMEI_DST_NAME}" "${BUILD_NUMBER}" "${TIER}" "ios"
write_download_info "Keyman for iPhone and iPad" "${KEYMANAPP_IPA_DST}" "${BUILD_NUMBER}" "${TIER}" "ios"

if [ ${RELEASE_OEM_FIRSTVOICES} = true ]; then
  write_download_info "FirstVoices Keyboards" "${FIRSTVOICESAPP_IPA_DST}" "${BUILD_NUMBER}" "${TIER}" "ios"
fi
