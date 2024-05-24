
WIXPATH="$ProgramFilesx86/WiX Toolset v3.11/bin"

# !IFDEF LINT
WIXLIGHTLINT=()
#!ELSE
# we suppress ICE82 because it reports spurious errors with merge module keymanengine to do with duplicate sequence numbers.  Safely ignored.
#WIXLIGHTLINT=(-sice:ICE82 -sice:ICE80)
#!ENDIF

if [[ $GO_FAST == 1 ]]; then
  # for debug builds, we turn off compression because it is so hideously slow
  # for test builds, we also turn off compression
  WIXLIGHTCOMPRESSION=-dcl:none
else
  WIXLIGHTCOMPRESSION=-dcl:high
fi

WIXCANDLE="$WIXPATH/candle.exe"
WIXLIGHT="$WIXPATH/light.exe"
# WIXLIT="$WIXPATH/lit.exe" -wx -nologo
WIXHEAT="$WIXPATH/heat.exe"

