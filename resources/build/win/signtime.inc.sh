if [[ -z ${SC_PFX_SHA1+x} ]]; then
  SC_PFX_SHA1="$COMMON_ROOT/tools/certificates/keymantest-sha1.pfx"
fi

if [[ -z ${SC_PFX_SHA256+x} ]]; then
  SC_PFX_SHA256="$COMMON_ROOT/tools/certificates/keymantest-sha256.pfx"
fi

if [[ -z ${SC_URL+x} ]]; then
  SC_URL="https://keyman.com/"
fi

if [[ -z ${SC_PWD+x} ]]; then
  SC_PWD=
fi

signtime() {
  # TODO: convert to bash script
  cmd //c "$KEYMAN_ROOT/common/windows/signtime.bat" "$@"
}

function verify-all-executable-signatures-in-folder() {
  # ignore return value from sigcheck; we validate the response with verify_signatures
  # "*" needs to be inside the quotes as it is passed as-is to sigcheck, not as an expansion
  # note: it seems that sigcheck rejects forward-slash in paths, so cygpath is our friend
  ("$SIGCHECK" -q -s -e -v -accepteula "$(cygpath -w "$1")\\*" || true) > sig1
  "$VERIFY_SIGNATURES" -d "$KEYMAN_ROOT/VERSION.md" < sig1
  rm -f sig1
}
