#!/bin/bash
# Publish to downloads.keyman.com. This script runs on TeamCity.
# Working directory has to be `linux`.

if [ -z "$TEAMCITY_VERSION" ]; then
    echo "This script is supposed to run on TeamCity only"
    exit 1
fi

TIER=`cat ../TIER.md`
BUILD_NUMBER=`cat ../VERSION.md`
BUILD_COUNTER="$((${BUILD_NUMBER##*.}))"
DATE=`date +%F`

UPLOAD_BASE="upload"
UPLOAD_FOLDER="${BUILD_NUMBER}"
UPLOAD_DIR="${UPLOAD_BASE}/${UPLOAD_FOLDER}"

# Set permissions as required on download site
echo "Setting upload file permissions for downloads.keyman.com"
chmod a+rx "${UPLOAD_DIR}"
chmod g+w "${UPLOAD_DIR}"

chmod g+rw ${UPLOAD_DIR}/*
chmod a+r  ${UPLOAD_DIR}/*

ARTIFACTS=(keyman-${BUILD_NUMBER}.tar.gz \
       ibus-kmfl-${BUILD_NUMBER}.tar.gz \
       kmflcomp-${BUILD_NUMBER}.tar.gz \
       libkmfl-${BUILD_NUMBER}.tar.gz)
NAMES=("Keyman for Linux"\
    "IBus KMFL" \
    "kmflcomp" \
    "libkmfl")
for i in "${!ARTIFACTS[@]}"; do
    TAR_GZ=${ARTIFACTS[$i]}
    # Construct .download_info
    HASH=`md5sum ${UPLOAD_DIR}/${TAR_GZ} | cut -d ' ' -f 1`
    SIZE=`stat --print="%s" ${UPLOAD_DIR}/${TAR_GZ}`
    # TODO: Truncate NAME
    DOWNLOAD_INFO=$( jq -n \
    --arg NAME "${NAMES[$i]}" \
    --arg BUILD_NUMBER "$BUILD_NUMBER" \
    --arg DATE "$DATE" \
    --arg TIER "$TIER" \
    --arg FILENAME "$TAR_GZ" \
    --arg HASH "$HASH" \
    --arg BUILD_COUNTER "$BUILD_COUNTER" \
    --arg SIZE "$SIZE" \
    '{name: $NAME, version: $BUILD_NUMBER, date: $DATE, platform: "linux", stability: $TIER, file: $FILENAME, md5: $HASH, type: "tar.gz", build: $BUILD_COUNTER, size: $SIZE}' )
    echo $DOWNLOAD_INFO | jq . >> ${UPLOAD_DIR}/${TAR_GZ}.download_info
done
