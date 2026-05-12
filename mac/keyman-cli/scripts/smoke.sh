#!/usr/bin/env bash
# Manual smoke test for the `keyman` CLI.
#
# Runs every check that can be verified from the shell, and prompts
# you to perform the visual checks (menu bar / live typing in TextEdit)
# that only a human can confirm.
#
# Usage: ./scripts/smoke.sh
# Requirements: a `keyman` binary built via `cargo build --release` and
# Keyman installed with the matching IMK patch (i.e. built from this
# branch and installed into ~/Library/Input Methods/).
set -u

cd "$(dirname "$0")/.."

KEYMAN=${KEYMAN:-./target/release/keyman}
if [[ ! -x "$KEYMAN" ]]; then
  echo "error: $KEYMAN not found or not executable. Run 'cargo build --release' first." >&2
  exit 2
fi

PASS=0
FAIL=0

step() {
  printf '\n==> %s\n' "$*"
}

pause() {
  printf '    [press RETURN once verified, or type 'fail' + RETURN to mark this step failed] '
  read -r reply
  if [[ "$reply" == "fail" ]]; then
    FAIL=$((FAIL + 1))
    printf '    [marked failed]\n'
  else
    PASS=$((PASS + 1))
  fi
}

ensure_exit() {
  local expected=$1
  local actual=$2
  local label=$3
  if [[ "$actual" -eq "$expected" ]]; then
    printf '    [ok] exit code %s (expected %s)\n' "$actual" "$expected"
    PASS=$((PASS + 1))
  else
    printf '    [FAIL] exit code %s (expected %s) for %s\n' "$actual" "$expected" "$label"
    FAIL=$((FAIL + 1))
  fi
}

step '1. Pre-state'
"$KEYMAN" status
"$KEYMAN" list
printf '    [visual check] Confirm one keyboard is starred (`*`) in the list above.\n'
pause

step '2. activate (idempotent)'
"$KEYMAN" activate
ensure_exit 0 $? 'activate'
"$KEYMAN" activate --json
ensure_exit 0 $? 'activate --json (no-op)'
printf '    [visual check] Open the menu-bar input-source picker. Confirm Keyman is the active IM.\n'
pause

step '3. status reflects activation'
"$KEYMAN" status

step '4. select to a different keyboard'
CURRENT_SELECTED=$("$KEYMAN" status --json | python3 -c 'import sys,json; d=json.load(sys.stdin); print(d.get("selected_keyboard",{}).get("id",""))')
printf '    Currently selected: %s\n' "$CURRENT_SELECTED"
LIST_IDS=$("$KEYMAN" list --json | python3 -c 'import sys,json; d=json.load(sys.stdin); print("\n".join(k["id"] for k in d["keyboards"]))')
TARGET=""
while IFS= read -r line; do
  if [[ -n "$line" && "$line" != "$CURRENT_SELECTED" ]]; then
    TARGET=$line
    break
  fi
done <<< "$LIST_IDS"
if [[ -z "$TARGET" ]]; then
  echo "    [skip] no second keyboard available to select against"
else
  printf '    Selecting: %s\n' "$TARGET"
  "$KEYMAN" select "$TARGET"
  ensure_exit 0 $? "select $TARGET"
  printf '    [visual check] Open TextEdit. Type a few keys. Confirm the *new* keyboard is in effect on the very next keystroke (no Keyman restart).\n'
  pause
  printf '    [visual check] Open Keyman menu. Confirm the menu checkmark has moved to the new selection.\n'
  pause
fi

step '5. error paths'
"$KEYMAN" select does-not-exist
ensure_exit 3 $? 'unknown keyboard'

# Selecting the current keyboard is idempotent.
"$KEYMAN" select "$TARGET" >/dev/null
ensure_exit 0 $? 'select current (idempotent)'

step '6. JSON outputs parse with jq'
if command -v jq >/dev/null 2>&1; then
  "$KEYMAN" list --json | jq -e '.keyboards | type == "array"' >/dev/null
  ensure_exit 0 $? 'list --json parses'
  "$KEYMAN" status --json | jq -e '.im_registered, .im_selected, .im_process_running | type == "boolean"' >/dev/null
  ensure_exit 0 $? 'status --json parses'
  "$KEYMAN" select "$TARGET" --json | jq -e '.selected.id == "'"$TARGET"'"' >/dev/null
  ensure_exit 0 $? 'select --json parses with correct id'
else
  echo "    [skip] jq not installed; skipping JSON shape checks"
fi

step '7. restore prior selection (optional)'
if [[ -n "$CURRENT_SELECTED" && "$CURRENT_SELECTED" != "$TARGET" ]]; then
  printf '    Restoring: %s\n' "$CURRENT_SELECTED"
  "$KEYMAN" select "$CURRENT_SELECTED"
fi

printf '\n==> done: %s passed, %s failed\n' "$PASS" "$FAIL"
exit $([[ "$FAIL" -eq 0 ]] && echo 0 || echo 1)
