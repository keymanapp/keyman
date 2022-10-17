#!/usr/bin/env bash

# Copyright:    © SIL International.
# Description:  Import new files from CLDR tech preview
# Create Date:  17 Oct 2022
# Authors:      Steven R. Loomis (SRL)

if [[ $# -ne 1 ]];
then
    echo >&2 "Usage: $0 <cldr-dir>"
    exit 1
fi

CLDR_DIR="$1"
shift

KEYBOARDS_DIR="${CLDR_DIR}/keyboards"
DTD_DIR="${KEYBOARDS_DIR}/dtd"
IMPORT_DIR="${KEYBOARDS_DIR}/import"

# a file to check
CHECK_1="${DTD_DIR}/ldmlKeyboard.dtd"
CHECK_2="${DTD_DIR}/ldmlKeyboardTest.dtd"

if [[ ! -f "${CHECK_1}" ]];
then
    echo >&2 "$0: error: ${CHECK_1} did not exist"
    echo >&2 "$0: error: Is ${CLDR_DIR} a valid cldr keyboard directory?"
    exit 1
fi

if [[ ! -f "${CHECK_2}" ]];
then
    echo >&2 "$0: error: ${CHECK_2} did not exist"
    echo >&2 "$0: error: Is ${CLDR_DIR} a valid cldr keyboard directory?"
    exit 1
fi

# collect git info
GIT_DESCRIBE=$(cd "${CLDR_DIR}" && git describe HEAD || echo unknown)
GIT_SHA=$(cd "${CLDR_DIR}" && git rev-parse HEAD || echo unknown)
NOW=$(date -u -R)

echo ${CLDR_DIR} - ${GIT_DESCRIBE} - ${GIT_SHA}
echo "---"

cp -v "${DTD_DIR}"/*.dtd "${DTD_DIR}"/*.xsd .
rsync -av --delete "${IMPORT_DIR}"/ ./import/

echo "{\"sha\": \""${GIT_SHA}"\",\"description\":\""${GIT_DESCRIBE}"\",\"date\":\""${NOW}"\"}" | jq . | tee cldr_info.json
echo "Updated cldr_info.json"

echo "Converting XSD to JSON…"

for xsd in *.xsd;
do
    base=$(basename ${xsd} .xsd | tr A-Z a-z | sed -e 's%^ldml%ldml-%g' )
    json=${base}.schema.json
    echo "${xsd} -> ${json}"
    (cd .. ; npx -p  jgexml xsd2json techpreview/${xsd} techpreview/${json}) || exit
    echo 'fixup-schema.js' ${json}
    node fixup-schema.js ${json} || exit
    mv ${json} tmp.json
    jq . -S < tmp.json > ${json} || (rm tmp.json ; exit)
    rm tmp.json
done
