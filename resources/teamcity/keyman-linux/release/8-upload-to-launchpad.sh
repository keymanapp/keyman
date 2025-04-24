#!/usr/bin/env bash
# Step name: Upload to launchpad

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

git reset --hard

/usr/lib/gnupg/gpg-preset-passphrase --passphrase "${GPGKEYPW}" --preset "${GPGKEYGRIP}"
UPLOAD=yes scripts/launchpad.sh
/usr/lib/gnupg/gpg-preset-passphrase --forget "${GPGKEYGRIP}" || true
