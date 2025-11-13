
WIX6PATH="C:\Users\ross_\.dotnet\tools"

# !IFDEF LINT
#WIXLIGHTLINT=()
#!ELSE
# we suppress ICE82 because it reports spurious errors with merge module keymanengine to do with duplicate sequence numbers.  Safely ignored.
#WIXLIGHTLINT=(-sice:ICE82 -sice:ICE80)
#!ENDIF

if [[ "$GO_FAST" == 1 ]]; then
  # for debug builds, we turn off compression because it is so hideously slow
  # for test builds, we also turn off compression
  WIXCOMPRESSION="-dcl none"
else
  WIXCOMPRESSION="-dcl high"
fi

# WIXLIT="$WIX6PATH/lit.exe" -wx -nologo
WIX6BUILD="$WIX6PATH/wix.exe build"
