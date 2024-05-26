# Delphi Flags: these flags may be imported into build scripts via the
# environment.inc.sh import.
#
# Note: there are probably some overlaps in usage for some of these
# macros, and we may wish to reorganize them later.

# This may need to be updated if any Delphi library paths change
DELPHIINCLUDES="$KEYMAN_ROOT/common/windows/delphi/ext/cef4delphi/source;$KEYMAN_ROOT/common/windows/delphi/ext/dcpcrypt;$KEYMAN_ROOT/common/windows/delphi/ext/jwa/Win32API;$KEYMAN_ROOT/common/windows/delphi/ext/sentry;$KEYMAN_ROOT/developer/src/ext/mbcolor;$KEYMAN_ROOT/developer/src/ext/scfontcombobox"

if builder_is_debug_build; then
  TARGET_PATH=Debug
else
  TARGET_PATH=Release
fi

DELPHI_MSBUILD_FLAG_DEBUG="//p:Config=$TARGET_PATH"

# Visual C++ x86, x64; Delphi x86
WIN32_TARGET_PATH=bin/Win32/$TARGET_PATH
X64_TARGET_PATH=bin/x64/$TARGET_PATH

# Delphi x86, x64
# WIN32_TARGET_PATH=...
WIN64_TARGET_PATH=bin/Win64/$TARGET_PATH
DEVELOPER_ROOT="$KEYMAN_ROOT/developer"
DEVELOPER_PROGRAM="$DEVELOPER_ROOT/bin"
DEVELOPER_OUTLIB="$DEVELOPER_ROOT/lib"
DEVELOPER_DEBUGPATH="$DEVELOPER_ROOT/debug"

# Delphi build tool macros

SENTRYTOOL="$COMMON_ROOT/tools/sentrytool/$WIN32_TARGET_PATH/sentrytool.exe"
TDS2DBG="$KEYMAN_ROOT/common/windows/bin/tools/tds2dbg"
DEVTOOLS="$COMMON_ROOT/tools/devtools/$WIN32_TARGET_PATH/devtools.exe"
