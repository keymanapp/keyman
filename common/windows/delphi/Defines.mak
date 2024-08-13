# DEBUG=1

# TODO: this should be a shared Defines.mak for common,developer,windows. So we
# need to move any project-specific stuff into a defines-windows.mak,
# defines-etc.mak

#
# Paths
#

!IFNDEF KEYMAN_ROOT
!ERROR KEYMAN_ROOT must be defined!
!ENDIF

# TODO: COMMON_ROOT should be common\windows
COMMON_ROOT=$(KEYMAN_ROOT)\common\windows\delphi
WINDOWS_ROOT=$(KEYMAN_ROOT)\windows
OUTLIB=$(WINDOWS_ROOT)\lib
COMMON_OUTLIB=$(KEYMAN_ROOT)\windows\lib
COMMON_BIN=$(KEYMAN_ROOT)\windows\bin

# INCLUDE=$(ROOT)\src\global\inc;$(INCLUDE)

!IFDEF DEBUG
GO_FAST=1
MAKEFLAG_DEBUG="DEBUG=$(DEBUG)"
DELPHI_MSBUILD_FLAG_DEBUG="/p:Config=Debug"
!ELSE
!IFDEF TEAMCITY_PR_NUMBER
GO_FAST=1
!ENDIF
DELPHI_MSBUILD_FLAG_DEBUG="/p:Config=Release"
!ENDIF

!IFDEF USERDEFINES
MAKEFLAG_USERDEFINES="USERDEFINES=$(USERDEFINES)"
!ENDIF

!IFDEF SC_TIMESTAMP
MAKEFLAG_SC_TIMESTAMP="SC_TIMESTAMP=$(SC_TIMESTAMP)"
!ENDIF

!IFDEF BUILDHELP
MAKEFLAG_BUILDHELP="BUILDHELP=$(BUILDHELP)"
!ENDIF

!IFDEF LINT
MAKEFLAG_LINT="LINT=$(LINT)"
!ENDIF

!IFDEF NOUI
MAKEFLAG_QUIET="NOUI=$(NOUI)"
!ELSE
!IFDEF QUIET
MAKEFLAG_QUIET="QUIET=$(QUIET)"
!ENDIF
!ENDIF

!IFDEF RELEASE_OEM
MAKEFLAG_RELEASE_OEM="RELEASE_OEM=$(RELEASE_OEM)"
!ENDIF

#
# USERDEFINES allows the developer to specify overrides for various settings. We need a variable
# because Makefiles cannot test for file existence
#

# TODO: can we eliminate this?

!ifdef USERDEFINES
!include $(WINDOWS_ROOT)\src\UserDefines.mak
!endif

#
# Delphi Compiler Configuration - Delphi 10.3.2
#

!IFNDEF DELPHI_VERSION
DELPHI_VERSION=20.0
!ENDIF

DCC32PATH=C:\Program Files (x86)\Embarcadero\Studio\$(DELPHI_VERSION)\bin

#
# Pass local configuration through to sub-instances of MAKE
#

MAKE="nmake" /C $(MAKEFLAG_QUICK_BUILD_KEYMAN) $(MAKEFLAG_USERDEFINES) $(MAKEFLAG_DEBUG) $(MAKEFLAG_BUILDHELP) $(MAKEFLAG_BUILDRTF) $(MAKEFLAG_SC_TIMESTAMP) $(MAKEFLAG_LINT) $(MAKEFLAG_QUIET) $(MAKEFLAG_RELEASE_OEM)

#
# Delphi build commands
#

!IFDEF DEBUG
TARGET_PATH=Debug
!ELSE
TARGET_PATH=Release
!ENDIF

!IFDEF LINT
DELPHIWARNINGS=-W+MESSAGE_DIRECTIVE -W+IMPLICIT_STRING_CAST -W+IMPLICIT_STRING_CAST_LOSS -W+EXPLICIT_STRING_CAST -W+EXPLICIT_STRING_CAST_LOSS -W+CVT_WCHAR_TO_ACHAR -W+CVT_NARROWING_STRING_LOST -W+CVT_ACHAR_TO_WCHAR -W+CVT_WIDENING_STRING_LOST -W+UNICODE_TO_LOCALE -W+LOCALE_TO_UNICODE -W+IMPLICIT_VARIANTS
!ELSE
DELPHIWARNINGS=-W-MESSAGE_DIRECTIVE -W-IMPLICIT_STRING_CAST -W-IMPLICIT_STRING_CAST_LOSS -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS -W-CVT_WCHAR_TO_ACHAR -W-CVT_NARROWING_STRING_LOST -W-CVT_ACHAR_TO_WCHAR -W-CVT_WIDENING_STRING_LOST -W-UNICODE_TO_LOCALE -W-LOCALE_TO_UNICODE -W-IMPLICIT_VARIANTS -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -W-COMBINING_SIGNED_UNSIGNED64 -W-COMBINING_SIGNED_UNSIGNED64
!ENDIF

DELPHIDPRPARAMS=-Q -B -GD -H -VT -^$C+ -^$D+ -^$J+ -^$L+ -^$O+ -^$Q- -^$R- -^$W+ -^$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -NU.\obj\Win32\$(TARGET_PATH) -E.\bin\Win32\$(TARGET_PATH)
DELPHIDPRPARAMS64=-Q -B -GD -H -VT -^$C+ -^$D+ -^$J+ -^$L+ -^$O+ -^$Q- -^$R- -^$W+ -^$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -NU.\obj\Win64\$(TARGET_PATH) -E.\bin\Win64\$(TARGET_PATH)
DELPHIDPKPARAMS=-Q -B -GD -VT -^$C+ -^$D+ -^$J+ -^$L+ -^$O+ -^$Q- -^$R- -^$W+ -^$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(OUTLIB) -LN$(OUTLIB) -NSData -NUobj\Win32\$(TARGET_PATH)

COMMON_DELPHIDPKPARAMS=-Q -B -GD -VT -^$C+ -^$D+ -^$J+ -^$L+ -^$O+ -^$Q- -^$R- -^$W+ -^$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(COMMON_OUTLIB) -LN$(COMMON_OUTLIB) -NSData -NUobj\Win32\$(TARGET_PATH)

# we are using cmd /c because dcc32/dcc64 are failing on direct execution
# from nmake
DCC32=cmd /c "$(DCC32PATH)\dcc32.exe" $(DELPHIDPRPARAMS)
DCC32DPK=cmd /c "$(DCC32PATH)\dcc32.exe" $(DELPHIDPKPARAMS)
COMMON_DCC32DPK=cmd /c "$(DCC32PATH)\dcc32.exe" $(COMMON_DELPHIDPKPARAMS)
DCC64=cmd /c "$(DCC32PATH)\dcc64.exe" $(DELPHIDPRPARAMS64) -N0x64\ -Ex64\

#
# Delphi MSBuild related commands and macros
#

DELPHI_MSBUILD="$(COMMON_ROOT)\tools\msbuild-wrapper.bat" "$(DCC32PATH)" $(DELPHI_MSBUILD_FLAG_DEBUG)

!IFDEF NODELPHI
DCC32=echo skipping
DCC32DPK=echo skipping
DCC64=echo skipping
DELPHI_MSBUILD=echo skipping
!ENDIF

# Visual C++ x86, x64
WIN32_TARGET_PATH=bin\Win32\$(TARGET_PATH)
X64_TARGET_PATH=bin\x64\$(TARGET_PATH)

# Delphi x86, x64
# WIN32_TARGET_PATH=...
WIN64_TARGET_PATH=bin\Win64\$(TARGET_PATH)

#
# Shared devtools app for common Delphi source manipulation
#

DEVTOOLS=$(COMMON_ROOT)\tools\devtools\$(WIN32_TARGET_PATH)\devtools.exe

#
# Other program build commands
#

BRCC32=rc.exe

HHC="C:\Program Files (x86)\HTML Help Workshop\hhc.exe"
NMAKE=nmake.exe
CL=cl.exe
MSBUILD=msbuild.exe
# /maxcpucount see https://devblogs.microsoft.com/cppblog/precompiled-header-pch-issues-and-recommendations/
MT=mt.exe
VCBUILD=error

!IFDEF DEBUG
MSBUILD_BUILD=/t:Build /p:Configuration=Debug
MSBUILD_CLEAN=/t:Clean /p:Configuration=Debug
!ELSE
MSBUILD_BUILD=/t:Rebuild /p:Configuration=Release
MSBUILD_CLEAN=/t:Clean /p:Configuration=Release
!ENDIF

COPY=copy
ISXBUILD=C:\PROGRA~1\INSTALLSHIELD\Express\System\IsExpCmdBld
WZZIPPATH="C:\program files\7-zip\7z.exe"
!IFDEF GO_FAST
WZZIP=$(WZZIPPATH) a -mx1
!ELSE
WZZIP=$(WZZIPPATH) a -mx9
!ENDIF
WZUNZIP=$(WZZIPPATH) e

# we are using cmd /c because tds2dbg is failing on direct execution
# from nmake
TDS2DBG=cmd /c $(KEYMAN_ROOT)\common\windows\bin\tools\tds2dbg
SENTRYTOOL=$(COMMON_ROOT)\tools\sentrytool\$(WIN32_TARGET_PATH)\sentrytool.exe
SENTRYTOOL_DELPHIPREP=$(SENTRYTOOL) delphiprep -r $(KEYMAN_ROOT) -i $(DELPHIINCLUDES)

WIXPATH="c:\program files (x86)\WiX Toolset v3.11\bin"
WIXCANDLE=$(WIXPATH)\candle.exe -wx -nologo

!IFDEF LINT
WIXLIGHTLINT=
!ELSE
# we suppress ICE82 because it reports spurious errors with merge module keymanengine to do with duplicate sequence numbers.  Safely ignored.
WIXLIGHTLINT= -sice:ICE82 -sice:ICE80
!ENDIF

!IFDEF GO_FAST
# for debug builds, we turn off compression because it is so hideously slow
# for test builds, we also turn off compression
WIXLIGHTCOMPRESSION=-dcl:none
!ELSE
WIXLIGHTCOMPRESSION=-dcl:high
!ENDIF

WIXLIGHT=$(WIXPATH)\light.exe -wx -nologo $(WIXLIGHTLINT) $(WIXLIGHTCOMPRESSION)

WIXLIT=$(WIXPATH)\lit.exe -wx -nologo
WIXHEAT=$(WIXPATH)\heat.exe

LINKPATH=link.exe

#
# Certificates and code signing
#

!ifdef SIGNCODE_BUILD
MAKE=$(MAKE) "SIGNCODE_BUILD=$(SIGNCODE_BUILD)"
!else
MAKE=$(MAKE)
!endif

#
# To get a .pfx from a .spc and .pvk, run pvk2pfx.exe
#

!IFNDEF SC_PFX_SHA1
SC_PFX_SHA1="$(COMMON_ROOT)\tools\certificates\keymantest-sha1.pfx"
!ENDIF

!IFNDEF SC_PFX_SHA256
SC_PFX_SHA256="$(COMMON_ROOT)\tools\certificates\keymantest-sha256.pfx"
!ENDIF

!IFNDEF SC_URL
SC_URL="https://keyman.com/"
!ENDIF

!IFNDEF SC_PWD
SC_PWD=""
!ENDIF

SIGNCODE=@"$(KEYMAN_ROOT)\common\windows\signtime.bat" signtool.exe $(SC_PFX_SHA1) $(SC_PFX_SHA256) $(SC_URL) $(SC_PWD)

#
# On some computers, the PLATFORM environment variable is set to x86. This can break msbuild
# with our projects. This may be resolvable in the future, but for now the easy fix is ...
#

PLATFORM=Win32

#
# mkver commands. mkver determines tag from the local build environment variables
# in the same way as /resources/build/build-utils.sh.
#

!ifdef GIT_BASH_FOR_KEYMAN
MKVER_SH=$(GIT_BASH_FOR_KEYMAN) $(KEYMAN_ROOT)\common\windows\mkver.sh
!else
MKVER_SH=start /wait $(KEYMAN_ROOT)\common\windows\mkver.sh
!endif

MKVER_M=$(MKVER_SH) manifest.in manifest.xml
MKVER_U=$(MKVER_SH)

#
# Symstore
#

# KEYMAN_SYMSTOREPATH defaults to sibling folder "symbols". If it is not present,
# then we won't attempt to write symbols to the store.
!IFNDEF KEYMAN_SYMSTOREPATH
KEYMAN_SYMSTOREPATH=$(KEYMAN_ROOT)\..\symbols
!ENDIF

# Nearly matches algorithm from resources/build/build-utils.sh
# For now, we'll use it only for SYMSTORE, where it is for reference
# only. Thus using the variable name __VERSION_WITH_TAG. Issues:
# 1. always appends tier, even for stable
# 2. test builds will append a branch name for master/beta/stable-x.y
# 3. this is only available for `make symbols` (VERSION_WIN, VERSION_TIER
#    are defined in Targets.mak only here)
# Fixing this properly would be possible but take a fair bit more
# work than I want to do just now. The intent is to make it possible to find
# symbols in the symstore index which we can purge later on.
__VERSION_WITH_TAG=$(VERSION_WIN)-$(VERSION_TIER)
!IFNDEF TEAMCITY_VERSION
__VERSION_WITH_TAG=$(__VERSION_WITH_TAG)-local
!ELSE
!IFDEF TEAMCITY_PR_NUMBER
__VERSION_WITH_TAG=$(__VERSION_WITH_TAG)-test-$(TEAMCITY_PR_NUMBER)
!ENDIF
!ENDIF

# This command depends on VERSION_WIN and VERSION_TIER being defined, through
# `make symbols` (i.e. don't call `make wrap-symbols`)
SYMSTORE="C:\Program Files (x86)\Windows Kits\10\Debuggers\x64\symstore.exe" add \
    /s "$(KEYMAN_SYMSTOREPATH)" \
    /v "$(VERSION_WIN)" \
    /c "Version: $(__VERSION_WITH_TAG)" \
    /compress /f

CLEAN=-del /S /Q
