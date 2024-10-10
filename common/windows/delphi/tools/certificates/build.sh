#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build and install test certificates" clean configure build test \
  "certificates   Create test certificates and install root certificates"
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh

#-------------------------------------------------------------------------------------------------------------------

function do_certificates() {
  rm -f KeymanTestCA-sha1.* KeymanTest-sha1.* KeymanTest-sha256.* KeymanTestCA-sha256.*

  source "$KEYMAN_ROOT/resources/build/win/visualstudio_environment.inc.sh"

  makecert -r -pe -n "CN=Keyman Test CA SHA1" -ss CA -sr CurrentUser -a sha1 -cy authority -sky signature -sv KeymanTestCA-sha1.pvk KeymanTestCA-sha1.cer
  certutil -user -addstore Root KeymanTestCA-sha1.cer
  makecert -pe -n "CN=Keyman Test Certificate SHA1" -a sha1 -cy end -sky signature -ic KeymanTestCA-sha1.cer -iv KeymanTestCA-sha1.pvk -sv KeymanTest-sha1.pvk KeymanTest-sha1.cer
  pvk2pfx -pvk KeymanTest-sha1.pvk -spc KeymanTest-sha1.cer -pfx KeymanTest-sha1.pfx

  makecert -r -pe -n "CN=Keyman Test CA" -ss CA -sr CurrentUser -a sha256 -cy authority -sky signature -sv KeymanTestCA-sha256.pvk KeymanTestCA-sha256.cer
  certutil -user -addstore Root KeymanTestCA-sha256.cer
  makecert -pe -n "CN=Keyman Test Certificate" -a sha256 -cy end -sky signature -ic KeymanTestCA-sha256.cer -iv KeymanTestCA-sha256.pvk -sv KeymanTest-sha256.pvk KeymanTest-sha256.cer
  pvk2pfx -pvk KeymanTest-sha256.pvk -spc KeymanTest-sha256.cer -pfx KeymanTest-sha256.pfx
}

#builder_run_action clean:project        do_clean
#builder_run_action configure:project    configure_windows_build_environment
#builder_run_action build:project        do_build
builder_run_action certificates:project do_certificates
# builder_run_action test:project         do_test
