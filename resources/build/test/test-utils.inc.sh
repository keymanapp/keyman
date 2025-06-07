function assert-equal() {
  local actual="$1"
  local expected="$2"
  local message=
  if [[ $# -gt 2 ]]; then
    message="$3: "
  fi

  if [[ "$actual" != "$expected" ]]; then
    builder_die "  ✕ FAIL: ${message}actual result '$actual' should equal expected '$expected'"
  else
    builder_echo green "  ✓ PASS: ${message}result '$actual' is correct"
  fi
}
