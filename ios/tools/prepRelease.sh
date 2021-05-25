#!/usr/bin/env bash

## Note - this script is designed solely for use within our Continuous
## Integration processes with `pwd` as this script's parent folder.

# Exit if program call fails
set -e

# Exit if unset variable used
## set -u (causes history-utils.sh to fail)

# Only allow one upload artifact set to exist.
if [ -d "upload" ]; then
  rm -rf upload
fi

#Include script dependency
. ../resources/build/history/history-utils.sh         #includes the following
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
get_version_notes "ios" "%build.number%" "$TIER" > $CHANGELOG_PATH
echo "* Minor fixes and performance improvements" >> $CHANGELOG_PATH
assertFileExists "${CHANGELOG_PATH}"

# Strip emoji to make Apple happy
emoji="\U1f300-\U1f5ff\U1f900-\U1f9ff\U1f600-\U1f64f\U1f680-\U1f6ff\U2600-\U26ff\U2700-\U27bf\U1f1e6-\U1f1ff\U1f191-\U1f251\U1f004\U1f0cf\U1f170-\U1f171\U1f17e-\U1f17f\U1f18e\U3030\U2b50\U2b55\U2934-\U2935\U2b05-\U2b07\U2b1b-\U2b1c\U3297\U3299\U303d\U00a9\U00ae\U2122\U23f3\U24c2\U23e9-\U23ef\U25b6\U23f8-\U23fa"
LC_ALL=UTF-8 sed -e "s/[$(printf $emoji)]//g" < "$CHANGELOG_PATH" > "$CHANGELOG_PATH.1"
mv -f "$CHANGELOG_PATH.1" "$CHANGELOG_PATH"
assertFileExists "${CHANGELOG_PATH}"

#
# Keyman Engine
#

KMEI_DST_NAME="keyman-engine-ios-${BUILD_NUMBER}.zip"
KMEI_DST="${WORK_DIR}/${UPLOAD_DIR}/${KMEI_DST_NAME}"

KMEI_UNI_BASE="build/Build/Products/Release-universal/"
KMEI_IOS_BASE="build/Build/Products/Release-iphoneos/"
FRAMEWORK="KeymanEngine.framework"
UNI_FRAMEWORK="KeymanEngine-universal.framework"

KEYMAN_SAMPLES="samples"

echo "engine dest: $KMEI_DST"

echo "Zipping ${UNI_FRAMEWORK} => ${UPLOAD_DIR}/${KMEI_DST}..."
cd "${KMEI_UNI_BASE}"
mv "./$FRAMEWORK" "./$UNI_FRAMEWORK"
zip -qrX ${KMEI_DST} ${UNI_FRAMEWORK}
cd $WORK_DIR
cd "${KMEI_IOS_BASE}"
zip -qrX ${KMEI_DST} ${FRAMEWORK}
cd $WORK_DIR

echo "Copying Keyman Engine samples into ${UPLOAD_DIR}/${KMEI_DST_NAME}..."
cp -rf "${KEYMAN_SAMPLES}" "${UPLOAD_DIR}/samples"
cd "${UPLOAD_DIR}"
zip -qr "${KMEI_DST_NAME}" "samples"
rm -rf "samples"
cd $WORK_DIR

#
# Keyman app
#

KEYMANAPP_IPA="build/Build/Products/Release-iphoneos/Keyman.ipa"
KEYMANAPP_IPA_DST="keyman-ios-${BUILD_NUMBER}.ipa"

echo "Copying Keyman IPA ${KEYMANAPP_IPA} => ${UPLOAD_DIR}/${KEYMANAPP_IPA/DST}..."
cp "${KEYMANAPP_IPA}" "${WORK_DIR}/${UPLOAD_DIR}/${KEYMANAPP_IPA_DST}"

#
# FirstVoices app
#

if [ ${RELEASE_OEM_FIRSTVOICES} = true ]; then
  FIRSTVOICESAPP_IPA="../oem/firstvoices/ios/build/Build/Products/Release-iphoneos/FirstVoices.ipa"
  FIRSTVOICESAPP_IPA_DST="firstvoices-ios-${BUILD_NUMBER}.ipa"

  echo "Copying FirstVoices IPA ${FIRSTVOICESAPP_IPA} => ${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA/DST}..."
  cp "${FIRSTVOICESAPP_IPA}" "${WORK_DIR}/${UPLOAD_DIR}/${FIRSTVOICESAPP_IPA_DST}"
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