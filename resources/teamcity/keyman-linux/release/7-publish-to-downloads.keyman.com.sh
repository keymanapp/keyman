#!/usr/bin/env bash
# Step name: Publish to downloads.keyman.com

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

TIER=$(cat ../TIER.md)
BUILD_NUMBER=$(cat ../VERSION.md)
BUILD_COUNTER="$((${BUILD_NUMBER##*.}))"
DATE=$(date +%F)

UPLOAD_BASE="upload"
UPLOAD_FOLDER="${BUILD_NUMBER}"
UPLOAD_DIR="${UPLOAD_BASE}/${UPLOAD_FOLDER}"

# Set permissions as required on download site
echo "Setting upload file permissions for downloads.keyman.com"
chmod a+rx "${UPLOAD_DIR}"
chmod g+w "${UPLOAD_DIR}"

chmod g+rw "${UPLOAD_DIR}"/*
chmod a+r  "${UPLOAD_DIR}"/*

ARTIFACTS=("keyman-${BUILD_NUMBER}.tar.gz")
NAMES=("Keyman for Linux")

for i in "${!ARTIFACTS[@]}"; do
    TAR_GZ=${ARTIFACTS[${i}]}
    # Construct .download_info
    HASH=$(md5sum "${UPLOAD_DIR}/${TAR_GZ}" | cut -d ' ' -f 1)
    SIZE=$(stat --print="%s" "${UPLOAD_DIR}/${TAR_GZ}")
    # TODO: Truncate NAME
    DOWNLOAD_INFO=$( jq -n \
    --arg NAME "${NAMES[${i}]}" \
    --arg BUILD_NUMBER "${BUILD_NUMBER}" \
    --arg DATE "${DATE}" \
    --arg TIER "${TIER}" \
    --arg FILENAME "${TAR_GZ}" \
    --arg HASH "${HASH}" \
    --arg BUILD_COUNTER "${BUILD_COUNTER}" \
    --arg SIZE "${SIZE}" \
    '{name: $NAME, version: $BUILD_NUMBER, date: $DATE, platform: "linux", stability: $TIER, file: $FILENAME, md5: $HASH, type: "tar.gz", build: $BUILD_COUNTER, size: $SIZE}' )
    echo "${DOWNLOAD_INFO}" | jq . >> "${UPLOAD_DIR}/${TAR_GZ}.download_info"
done

# Expanded, documented form of the arguments
# ==========================================
# "-vrzltp "   # verbose, recurse, zip, copy symlinks, preserve times, permissions
# "--perms "   # perfectly matches existing file permissions on the build agent
# "--stats "   # show statistics for log
# "--rsync-path=\"sudo -u vu2009 rsync\" # path on remote server
# "--rsh=ssh " # use ssh

# The actual rsync call.
# We run into weird quote-based issues if we don't do a monolithic call as seen below, at least at present.
echo "Performing rsync call."
rsync -vrzltp --perms --stats --rsync-path="%downloads_rsync_path%" --rsh=ssh "${UPLOAD_DIR}" %downloads_rsync_user%@%downloads_rsync_host%:%downloads_rsync_root%/linux/"${TIER}"
