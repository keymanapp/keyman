function assert-equal() {
  local actual="$1"
  local expected="$2"
  local message=
  if [[ $# -gt 2 ]]; then
    message="$3: "
  fi

  if [[ "$actual" != "$expected" ]]; then
    builder_die "FAIL: ${message}expected actual '$actual' to equal expected '$expected'"
  else
    builder_echo green "PASS: ${message}expected actual '$actual' to be equal to expected '$expected'"
  fi
}
