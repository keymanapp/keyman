#!/usr/bin/env bash

#
# This script finds all references to strings in strings.xml in order to make sure that old
# strings do not proliferate over time. The script generates the following files:
#
# * strings.txt: A plain-text list of all string identifiers found in strings.xml.
# * summary.md: A summary of number of matches for each string found.
# * details.md: List of every reference to each string found
#
# Note that you should review the results with care; some strings may not ever be found,
# because they may be metadata such as "author of translation", or they may have been
# added as part of upcoming functionality; they may also be found in temporary files and
# build artifacts. Because of this, and because the script takes a little while to run,
# I'm not adding the script to the standard build at this time.
#

set -e # die on non-zero exit code
set -u # die on undefined variables

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Preparation

echo "# List of localized string usage" > detail.md
echo "Collected at: $(date)" >> detail.md
echo >> detail.md

echo "# Summary of localized string usage" | tee summary.md
echo "Collected at: $(date)" | tee -a summary.md
echo | tee -a summary.md
echo " Count | String | Flag" | tee -a summary.md
echo "-------|--------|------" | tee -a summary.md

# Lazy extract all identifiers from strings.xml into local strings.txt
# Note that this relies on the XML following a single line per string pattern
# (up to the name attribute, anyway) but this is good enough for now.

grep -oP '<string.+name="(.+?)"' "$KEYMAN_ROOT/windows/src/desktop/kmshell/xml/strings.xml" | grep -oP 'Id="(.+?)"' | awk -F[\"] '{print $2}' > strings.txt

while IFS= read -r string; do
  echo "## $string" >> detail.md
  count=$(grep -Inr \
    --exclude=locale.xml --exclude=strings.xml \
    --exclude-dir=i18n-check-unused-strings --exclude=messages.txt --exclude=MessageIdentifierConsts.pas \
    --exclude-dir=__history \
    "$string" \
    "$KEYMAN_ROOT/windows/src/desktop" "$KEYMAN_ROOT/windows/src/engine" | tee -a detail.md | wc -l)
  echo >> detail.md

  # Write summary
  if [ $count -eq 0 ]; then
    echo "**0** | **$string** | **Not found**" | tee -a summary.md
  else
    echo "$count | $string" | tee -a summary.md
  fi

done < strings.txt
