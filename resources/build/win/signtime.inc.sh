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

