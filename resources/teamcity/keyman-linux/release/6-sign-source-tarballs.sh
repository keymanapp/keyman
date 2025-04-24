#!/usr/bin/env bash
# Step name: Sign source tarballs

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "${REPO_ROOT}/linux" || exit 1

# We do this after a rsync happened, because that starts the gpg-agent that is required
# for gpg-preset-passphrase to work
RELEASE_VERSION=$(cat ../VERSION.md)
cd "upload/${RELEASE_VERSION}" || exit 1

eval "$(gpg-agent -vv --daemon --allow-preset-passphrase --debug-level 9)"
/usr/lib/gnupg/gpg-preset-passphrase --passphrase "${GPGKEYPW}" --preset "${GPGKEYGRIP}"
for f in ./*.tar.gz; do gpg --output "${f}.asc" -a --detach-sig "${f}"; done
/usr/lib/gnupg/gpg-preset-passphrase --forget "${GPGKEYGRIP}" || true
