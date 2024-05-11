
WZZIPPATH="$ProgramFiles/7-zip/7z.exe"

wzzip() {
  if [[ $GO_FAST == 1 ]]; then
    "$WZZIPPATH" a -mx1 "$@"
  else
    "$WZZIPPATH" a -mx9 "$@"
  fi
}

wzunzip() {
  "$WZZIPPATH" e "$@"
}