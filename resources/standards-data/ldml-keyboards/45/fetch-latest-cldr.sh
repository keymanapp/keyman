#!/usr/bin/env bash

# Copyright:    © SIL International.
# Description:  Import new files from CLDR tech preview
# Create Date:  17 Oct 2022
# Authors:      Steven R. Loomis (SRL)

set -eu

if [[ $# -ne 1 ]];
then
    echo >&2 "Usage: $0 <cldr-dir>"
    exit 1
fi

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

cd "$THIS_SCRIPT_PATH"

CLDR_DIR="$1"
shift

KEYBOARDS_DIR="${CLDR_DIR}/keyboards"
DTD_DIR="${KEYBOARDS_DIR}/dtd"
IMPORT_DIR="${KEYBOARDS_DIR}/import"
DATA_DIR="${KEYBOARDS_DIR}/3.0"
TEST_DIR="${KEYBOARDS_DIR}/test"

# a file to check
CHECK_1="${DTD_DIR}/ldmlKeyboard3.dtd"      # Critical, present in prior CLDR
CHECK_2="${DTD_DIR}/ldmlKeyboardTest3.dtd"  # Only in Keyboard 3.0+

if [[ ! -f "${CHECK_1}" ]];
then
    builder_die "${CHECK_1} did not exist: is ${CLDR_DIR} a valid CLDR keyboard directory?"
fi

if [[ ! -f "${CHECK_2}" ]];
then
    builder_die "${CHECK_2} did not exist: is ${CLDR_DIR} a valid CLDR keyboard directory?"
fi

# collect git info
GIT_DESCRIBE=$(cd "${CLDR_DIR}" && git describe HEAD || echo unknown)
GIT_SHA=$(cd "${CLDR_DIR}" && git rev-parse HEAD || echo unknown)
NOW=$(date -u -R)

echo "${CLDR_DIR}" - "${GIT_DESCRIBE}" - "${GIT_SHA}"
echo "---"

# delete the old files in case some were removed from CLDR
rm -rf ./import ./3.0 ./dtd ./test
# copy over everything
cp -Rv "${IMPORT_DIR}" "${DATA_DIR}" "${DTD_DIR}" "${TEST_DIR}" .

# delete old files, no reason to keep them
rm -vf dtd/{ldmlKeyboard,ldmlPlatform}.{xsd,dtd}


echo "{\"sha\": \"${GIT_SHA}\",\"description\":\"${GIT_DESCRIBE}\",\"date\":\"${NOW}\"}" | ${JQ} . | tee cldr_info.json
echo "Updated cldr_info.json"

echo "Converting XSD to JSON…"

for xsd in dtd/*.xsd;
do
    base=$(basename "${xsd}" .xsd | tr A-Z a-z | sed -e 's%^ldml%ldml-%g' )
    json=${base}.schema.json
    echo "${xsd} -> ${json}"
    (cd .. ; npx -p  jgexml xsd2json 45/"${xsd}" 45/"${json}") || exit
    echo 'fixup-schema.js' "${json}"
    node fixup-schema.js "${json}" || builder_die "failed to fixup schema ${json}"
    mv "${json}" tmp.json
    ${JQ} . -S < tmp.json > "${json}" || (rm tmp.json ; builder_die "failed to transform final schema ${json}")
    rm tmp.json
done
