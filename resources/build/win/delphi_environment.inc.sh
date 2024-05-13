# Delphi build environment: defines the path and other variables that are
# required to run a Delphi build. The delphi_environment_generated.inc.sh
# file includes variables that are local to the computer they were defined
# on.
#
# These variables are only used within a Delphi build environment. This file
# should not be imported into a build.sh script, but only into a subshell, to
# avoid conflicts with a Visual Studio build environment.

# !IFDEF LINT
# DELPHIWARNINGS=-W+MESSAGE_DIRECTIVE -W+IMPLICIT_STRING_CAST -W+IMPLICIT_STRING_CAST_LOSS -W+EXPLICIT_STRING_CAST -W+EXPLICIT_STRING_CAST_LOSS -W+CVT_WCHAR_TO_ACHAR -W+CVT_NARROWING_STRING_LOST -W+CVT_ACHAR_TO_WCHAR -W+CVT_WIDENING_STRING_LOST -W+UNICODE_TO_LOCALE -W+LOCALE_TO_UNICODE -W+IMPLICIT_VARIANTS
# !ELSE
DELPHIWARNINGS=(-W-MESSAGE_DIRECTIVE -W-IMPLICIT_STRING_CAST -W-IMPLICIT_STRING_CAST_LOSS -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS -W-CVT_WCHAR_TO_ACHAR -W-CVT_NARROWING_STRING_LOST -W-CVT_ACHAR_TO_WCHAR -W-CVT_WIDENING_STRING_LOST -W-UNICODE_TO_LOCALE -W-LOCALE_TO_UNICODE -W-IMPLICIT_VARIANTS -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -W-COMBINING_SIGNED_UNSIGNED64 -W-COMBINING_SIGNED_UNSIGNED64)
# !ENDIF

DELPHI_VERSION=20.0
DCC32PATH="$(cygpath -u "$ProgramFilesx86\\Embarcadero\\Studio\\$DELPHI_VERSION\\bin")"

source "$KEYMAN_ROOT/resources/build/win/delphi_environment_generated.inc.sh"

DELPHIDPRPARAMS64=(-Q -B -GD -H -VT -\$C+ -\$D+ -\$J+ -\$L+ -\$O+ -\$Q- -\$R- -\$W+ -\$Y+ -E. \
  "${DELPHIWARNINGS[@]}" "-I$DELPHIINCLUDES" "-U$DELPHIINCLUDES" "-R$DELPHIINCLUDES" \
  "-NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win" \
  "-NU./obj/Win64/$TARGET_PATH" "-E./bin/Win64/$TARGET_PATH")

DELPHIDPKPARAMS=(-Q -B -GD -VT -\$C+ -\$D+ -\$J+ -\$L+ -\$O+ -\$Q- -\$R- -\$W+ -\$Y+ -E. \
  "${DELPHIWARNINGS[@]}" "-I$DELPHIINCLUDES" "-U$DELPHIINCLUDES" "-R$DELPHIINCLUDES" \
  "-NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win" \
  "-LE$OUTLIB" "-LN$OUTLIB" -NSData "-NUobj/Win32/$TARGET_PATH")

# COMMON_DELPHIDPKPARAMS=(-Q -B -GD -VT -\$C+ -\$D+ -\$J+ -\$L+ -\$O+ -\$Q- -\$R- -\$W+ -\$Y+ -E. \
#   "${DELPHIWARNINGS[@]}" "-I$DELPHIINCLUDES" "-U$DELPHIINCLUDES" "-R$DELPHIINCLUDES" \
#   -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win \
#   "-LE$COMMON_OUTLIB" "-LN$COMMON_OUTLIB" -NSData "-NUobj/Win32/$TARGET_PATH")

DEVELOPER_DELPHIDPKPARAMS=(-Q -B -GD -VT -\$C+ -\$D+ -\$J+ -\$L+ -\$O+ -\$Q- -\$R- -\$W+ -\$Y+ -E. \
  "${DELPHIWARNINGS[@]}" "-I$DELPHIINCLUDES" "-U$DELPHIINCLUDES" "-R$DELPHIINCLUDES" \
  "-NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win" \
  "-LE$DEVELOPER_OUTLIB" "-LN$DEVELOPER_OUTLIB" -NSData -NUobj/Win32/$TARGET_PATH)

DEVELOPER_DCC32DPK=("$DCC32PATH/dcc32.exe" "${DEVELOPER_DELPHIDPKPARAMS[@]}")

