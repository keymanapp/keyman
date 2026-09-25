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
