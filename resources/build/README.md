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

### builder-basic.inc.sh, builder-full.inc.sh

There are two wrappers for builder.inc.sh:

* resources/build/builder-basic.inc.sh - Keyman-specific builder-based script
  that also sets version environment,  repo base path, and other environment
  variables
* resources/build/builder-full.inc.sh - Keyman-specific builder-based script,
  calls builder-basic.inc.sh, sets cwd to script path, and on exit verifies that
  builder functions were used appropriately.

All build.sh scripts should use builder-full.inc.sh. Some helper scripts may
use builder-basic.inc.sh. No scripts in this repo (apart from tests) should
use builder.inc.sh directly.

builder.inc.sh is documented in [builder.md](../../docs/builder.md)

### jq/jq-win64

jq resp. jq-win64.exe is used by various builds for json query.

Official source: <https://stedolan.github.io/jq/>.
[License](jq-license.txt)
