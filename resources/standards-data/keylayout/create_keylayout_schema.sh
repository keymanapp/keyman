#!/usr/bin/env bash

# Copyright:    © SIL International.
# Description:  create a *.schema.json file from a *.XSD file
# Create Date:  20 May 2025
# Authors:      S. Schmitt


set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

cd "$THIS_SCRIPT_PATH"

 DTD_DIR="${THIS_SCRIPT_PATH}/dtd"

# a file to check
 CHECK_1="${DTD_DIR}/keylayout.xsd"   

 if [[ ! -f "${CHECK_1}" ]];
 then
     builder_die "${CHECK_1} does not exist"
 fi

 builder_echo heading "Converting XSD to JSON…" 
for xsd in dtd/*.xsd;
do
    base=$(basename "${xsd}" .xsd | tr A-Z a-z )
    json=${base}.schema.json
    builder_echo debug "${xsd} -> ${json}"
    (cd .. ; npx -p  jgexml xsd2json ${THIS_SCRIPT_PATH}/"${xsd}"  ${THIS_SCRIPT_PATH}/"${json}") || exit

    mv "${json}" tmp.json
    # reformat with prettier(JQ)
    ${JQ} . -S < tmp.json > "${json}" || (rm tmp.json ; builder_die "failed to transform final schema ${json}")
    rm tmp.json
done

echo
builder_echo success "Keylayout copied correctly from ${DTD_DIR}"
echo
