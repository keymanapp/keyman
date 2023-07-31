# Release builds

A release is triggered nightly whenever a pull request has been merged to the branch; it
runs from a TeamCity build configuration that runs `increment-version.sh` and then uploads
the resulting `HISTORY.md` to <https://downloads.keyman.com/history/HISTORY.md>.

To manually make a release (for example if a build falls over for a transient error or
there was a build configuration problem), create a new PR that targets the branch,
merge it, and then run the **Trigger Release Builds** build configuration for that
branch. If you have no changes, see that as an opportunity to improve documentation,
including this file...

## Various tools

### build-utils.sh

See [build-utils.md](build-utils.md)

### jq-win64

jq-win64.exe is used by various builds for json query.

Official source: <https://stedolan.github.io/jq/>.
[License](jq-license.txt)
