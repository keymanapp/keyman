#!/usr/bin/env bash

# Copyright:    © SIL International.
# Description:  Import new files from CLDR tech preview
# Create Date:  17 Oct 2022
# Authors:      Steven R. Loomis (SRL)

set -eu

if [[ $# -ne 2 ]];
then
    echo >&2 "Usage: $0 <cldr-major> <cldr-dir>"
    exit 1
fi

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

cd "$THIS_SCRIPT_PATH"

KEYMAN_CORE_LDML="../../../core/include/ldml"

builder_echo debug "Building ${KEYMAN_CORE_LDML}"

"${KEYMAN_CORE_LDML}/build.sh" || builder_die "building core/include/ldml failed"

KEYMAN_CORE_LDML_JS="${KEYMAN_CORE_LDML}/build/keyman_core_ldml.js"

if [ ! -f "${KEYMAN_CORE_LDML_JS}" ];
then
    builder_die "Could not read ${KEYMAN_CORE_LDML_JS} - check build."
fi

CLDR_VERSION="$1"
shift

CLDR_DIR="$1"
shift

node verify-version.mjs "${CLDR_VERSION}" || builder_die "Check CLDR version"

KEYBOARDS_DIR="${CLDR_DIR}/keyboards"
DTD_DIR="${KEYBOARDS_DIR}/dtd"
IMPORT_DIR="${KEYBOARDS_DIR}/import"
DATA_DIR="${KEYBOARDS_DIR}/3.0"
TEST_DIR="${KEYBOARDS_DIR}/test"
ABNF_DIR="${KEYBOARDS_DIR}/abnf"

# a file to check
CHECK_1="${DTD_DIR}/ldmlKeyboard3.dtd"            # Critical, present in prior CLDR
CHECK_2="${DTD_DIR}/ldmlKeyboardTest3.dtd"        # Only in Keyboard 3.0+
CHECK_3="${ABNF_DIR}/transform-from-required.abnf" # Present in v47+

if [[ ! -f "${CHECK_1}" ]];
then
    builder_die "${CHECK_1} did not exist: is ${CLDR_DIR} a valid CLDR keyboard directory?"
fi

if [[ ! -f "${CHECK_2}" ]];
then
    builder_die "${CHECK_2} did not exist: is ${CLDR_DIR} a valid CLDR keyboard directory?"
fi

if [[ ! -f "${CHECK_3}" ]];
then
    builder_die "${CHECK_3} did not exist: does ${CLDR_DIR} contain CLDR 47+? Or did ABNF change?"
fi


# collect git info
GIT_DESCRIBE=$(cd "${CLDR_DIR}" && git describe HEAD || echo unknown)
GIT_SHA=$(cd "${CLDR_DIR}" && git rev-parse HEAD || echo unknown)
NOW=$(date -u -R)


builder_echo heading "Ready to copy v${CLDR_VERSION} from ${CLDR_DIR}"
builder_echo debug "${GIT_DESCRIBE} - ${GIT_SHA}"

mkdir -pv "${CLDR_VERSION}"

cd "${CLDR_VERSION}"

pwd

# delete the old files in case some were removed from CLDR
rm -rf ./import ./3.0 ./dtd ./test
# copy over everything
cp -Rv "${IMPORT_DIR}" "${DATA_DIR}" "${DTD_DIR}" "${TEST_DIR}" "${ABNF_DIR}" .

# delete old files, no reason to keep them
rm -vf dtd/{ldmlKeyboard,ldmlPlatform}.{xsd,dtd}


echo "{\"sha\": \"${GIT_SHA}\",\"description\":\"${GIT_DESCRIBE}\",\"date\":\"${NOW}\"}" | ${JQ} . | tee cldr_info.json
builder_echo "Updated cldr_info.json"

builder_echo heading "Converting XSD to JSON…"

for xsd in dtd/*.xsd;
do
    base=$(basename "${xsd}" .xsd | tr A-Z a-z | sed -e 's%^ldml%ldml-%g' )
    json=${base}.schema.json
    builder_echo debug "${xsd} -> ${json}"
    (cd .. ; npx -p  jgexml xsd2json ${CLDR_VERSION}/"${xsd}" ${CLDR_VERSION}/"${json}") || exit
    builder_echo debug 'fixup-schema.js' "${json}"
    node ../fixup-schema.js "${json}" || builder_die "failed to fixup schema ${json}"
    mv "${json}" tmp.json
    ${JQ} . -S < tmp.json > "${json}" || (rm tmp.json ; builder_die "failed to transform final schema ${json}")
    rm tmp.json
done

# verify that the CLDR version we just converted is the requested version.
FOUND_VERSION=$(jq -r '.definitions.version.properties.cldrVersion.enum[0]' < ldml-keyboard3.schema.json)

if [ ${FOUND_VERSION} -ne ${CLDR_VERSION} ];
then
    # It's a little late to cleanup more gracefully, but we can at least complain.
    builder_die "You requested CLDR ${CLDR_VERSION} but the copied schema is for ${FOUND_VERSION} - something is wrong."
fi

echo
builder_echo success "CLDR ${CLDR_VERSION} copied correctly from ${CLDR_DIR}"
echo
