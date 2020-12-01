# DEBUG=1

#
# Paths
#

!IFNDEF KEYMAN_ROOT
KEYMAN_ROOT=c:\keyman
!ENDIF

ROOT=$(KEYMAN_ROOT)\windows

EXT=$(ROOT)\src\ext

INCLUDE=$(ROOT)\src\global\inc;$(INCLUDE)

!ifndef EXCLUDEPATHDEFINES
!include $(ROOT)\src\PathDefines.mak
!endif

PROGRAM=$(ROOT)\bin
BUILD=$(ROOT)\build
DEBUGPATH=$(ROOT)\debug
OUTLIB=$(ROOT)\lib

INSTALLPATH_KEYMANDESKTOP=%ProgramFiles(X86)%\Keyman\Keyman for Windows
INSTALLPATH_KEYMANDEVELOPER=%ProgramFiles(X86)%\Keyman\Keyman Developer
INSTALLPATH_KEYMANENGINE=%CommonProgramFiles(X86)%\Keyman\Keyman Engine

!IFDEF DEBUG
  MAKEFLAG_DEBUG=-DDEBUG
  DELPHI_MSBUILD_FLAG_DEBUG=/p:Config=Debug
!ELSE
  DELPHI_MSBUILD_FLAG_DEBUG=/p:Config=Release
!ENDIF

!IFDEF USERDEFINES
  MAKEFLAG_USERDEFINES=-DUSERDEFINES
!ENDIF

!IFDEF SC_TIMESTAMP
  MAKEFLAG_SC_TIMESTAMP=-DSC_TIMESTAMP
!ENDIF

!IFDEF BUILDHELP
  MAKEFLAG_BUILDHELP=-DBUILDHELP
!ENDIF

!IFDEF BUILDRTF
  MAKEFLAG_BUILDRTF=-DBUILDRTF
!ENDIF

!IFDEF REL_SUFFIX
!ERROR Not using REL_SUFFIX any more!
!ENDIF

!IFDEF LINT
  MAKEFLAG_LINT=-DLINT
!ENDIF

!IFDEF NOUI
  MAKEFLAG_QUIET=-DNOUI
!ELSE
!IFDEF QUIET
  MAKEFLAG_QUIET=-DQUIET
!ENDIF
!ENDIF

!IFDEF RELEASE_OEM
  MAKEFLAG_RELEASE_OEM=-DRELEASE_OEM
!ENDIF

!IFDEF QUICK_BUILD_KEYMAN
  MAKEFLAG_QUICK_BUILD_KEYMAN=-DQUICK_BUILD_KEYMAN
!ENDIF

#
# USERDEFINES allows the developer to specify overrides for various settings. We need a variable
# because Makefiles cannot test for file existence
#

!ifdef USERDEFINES
!include $(ROOT)\src\UserDefines.mak
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

MAKE="$(DCC32PATH)\make" -l $(MAKEFLAG_QUICK_BUILD_KEYMAN) $(MAKEFLAG_USERDEFINES) $(MAKEFLAG_DEBUG) $(MAKEFLAG_BUILDHELP) $(MAKEFLAG_BUILDRTF) $(MAKEFLAG_SC_TIMESTAMP) $(MAKEFLAG_LINT) $(MAKEFLAG_QUIET) $(MAKEFLAG_RELEASE_OEM)

#
# Delphi build commands
#

# DEVTOOLS=$(ROOT)\src\buildtools\devtools\devtools.exe
DEVTOOLS=$(PROGRAM)\buildtools\devtools.exe

!IFDEF LINT
DELPHIWARNINGS=-W+MESSAGE_DIRECTIVE -W+IMPLICIT_STRING_CAST -W+IMPLICIT_STRING_CAST_LOSS -W+EXPLICIT_STRING_CAST -W+EXPLICIT_STRING_CAST_LOSS -W+CVT_WCHAR_TO_ACHAR -W+CVT_NARROWING_STRING_LOST -W+CVT_ACHAR_TO_WCHAR -W+CVT_WIDENING_STRING_LOST -W+UNICODE_TO_LOCALE -W+LOCALE_TO_UNICODE -W+IMPLICIT_VARIANTS
!ELSE
DELPHIWARNINGS=-W-MESSAGE_DIRECTIVE -W-IMPLICIT_STRING_CAST -W-IMPLICIT_STRING_CAST_LOSS -W-EXPLICIT_STRING_CAST -W-EXPLICIT_STRING_CAST_LOSS -W-CVT_WCHAR_TO_ACHAR -W-CVT_NARROWING_STRING_LOST -W-CVT_ACHAR_TO_WCHAR -W-CVT_WIDENING_STRING_LOST -W-UNICODE_TO_LOCALE -W-LOCALE_TO_UNICODE -W-IMPLICIT_VARIANTS -W-IMPLICIT_INTEGER_CAST_LOSS -W-IMPLICIT_CONVERSION_LOSS -W-COMBINING_SIGNED_UNSIGNED64 -W-COMBINING_SIGNED_UNSIGNED64
!ENDIF

DELPHIDPRPARAMS=-Q -B -GD -H -VT -$C+ -$D+ -$L+ -$O+ -$Q- -$R- -$W+ -$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win
DELPHIDPRPARAMS64=-Q -B -GD -H -VT -$C+ -$D+ -$L+ -$O+ -$Q- -$R- -$W+ -$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win
DELPHIDPKPARAMS=-Q -B -GD -VT -$C+ -$D+ -$L+ -$O+ -$Q- -$R- -$W+ -$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(OUTLIB) -LN$(OUTLIB) -NSData

!IFDEF NOUI
DCC32="$(DCC32PATH)\dcc32.exe" $(DELPHIDPRPARAMS)
DCC32DPK="$(DCC32PATH)\dcc32.exe" $(DELPHIDPKPARAMS)
!ELSE
!IFDEF QUIET
DCC32=@$(DEVTOOLS) -dccq  $(DELPHIDPRPARAMS)
DCC32DPK=@$(DEVTOOLS) -dccq $(DELPHIDPKPARAMS)
!ELSE
DCC32=@$(DEVTOOLS) -dcc $(DELPHIDPRPARAMS)
DCC32DPK=@$(DEVTOOLS) -dcc $(DELPHIDPKPARAMS)
!ENDIF
!ENDIF

DCC64="$(DCC32PATH)\dcc64.exe" $(DELPHIDPRPARAMS64) -N0x64\ -Ex64\

#
# Delphi MSBuild related commands and macros
#

# Warning: whitespace is horribly significant in the following macro -- particularly lack of space before &&
DELPHI_MSBUILD=set DCC32PATH=$(DCC32PATH)&& $(ROOT)\src\buildtools\msbuild-wrapper.bat $(DELPHI_MSBUILD_FLAG_DEBUG)

!IFDEF DEBUG
TARGET_PATH=Debug
!ELSE
TARGET_PATH=Release
!ENDIF

WIN32_TARGET_PATH=Win32\$(TARGET_PATH)
WIN64_TARGET_PATH=Win64\$(TARGET_PATH)
X64_TARGET_PATH=x64\$(TARGET_PATH)

#
# Other program build commands
#

PHPDIR=\php
PHPEXE=$(PHPDIR)\php.exe

BRCC32=rc.exe

HHC="C:\Program Files (x86)\HTML Help Workshop\hhc.exe"
NMAKE=nmake.exe
CL=cl.exe
MSBUILD=msbuild.exe
# /maxcpucount see https://devblogs.microsoft.com/cppblog/precompiled-header-pch-issues-and-recommendations/
MT=mt.exe
VCBUILD=error

!IFDEF DEBUG
  MSBUILD_TARGET=Debug
  MSBUILD_BUILD=/t:Build /p:Configuration=Debug
  MSBUILD_CLEAN=/t:Clean /p:Configuration=Debug
!ELSE
  MSBUILD_TARGET=Release
  MSBUILD_BUILD=/t:Rebuild /p:Configuration=Release
  MSBUILD_CLEAN=/t:Clean /p:Configuration=Release
!ENDIF

COPY=copy
ISXBUILD=C:\PROGRA~1\INSTALLSHIELD\Express\System\IsExpCmdBld
WZZIPPATH="C:\program files\7-zip\7z.exe"
WZZIP=$(WZZIPPATH) a
WZUNZIP=$(WZZIPPATH) e

XSLTPROC=$(ROOT)\src\ext\libxslt\xsltproc.exe

# TDSPACK=error! $(ROOT)\src\buildtools\tdspack\tdspack -e
# TDSPACKCOMPRESS=error! $(ROOT)\src\buildtools\tdspack\tdspack -o

TDS2DBG=$(ROOT)\bin\buildtools\tds2dbg
SENTRYTOOL=$(ROOT)\bin\buildtools\sentrytool
SENTRYTOOL_DELPHIPREP=$(SENTRYTOOL) delphiprep -r $(KEYMAN_ROOT) -i $(DELPHIINCLUDES)

WIXPATH="c:\program files (x86)\WiX Toolset v3.11\bin"
WIXCANDLE=$(WIXPATH)\candle.exe -wx -nologo

!IFDEF LINT
WIXLIGHT=$(WIXPATH)\light.exe -wx -nologo
!ELSE
# we suppress ICE82 because it reports spurious errors with merge module keymanengine to do with duplicate sequence numbers.  Safely ignored.
WIXLIGHT=$(WIXPATH)\light.exe -wx -nologo -sice:ICE82 -sice:ICE80
!ENDIF

WIXLIT=$(WIXPATH)\lit.exe -wx -nologo
WIXHEAT=$(WIXPATH)\heat.exe

LINKPATH=link.exe

BACKUPDEFAULTS=*.map *.tds *.rsm *.dbg *.pdb *.cod

TORTOISEPROC=C:\Progra~1\TortoiseSVN\bin\TortoiseProc.exe

#
# Certificates and code signing
#

!ifdef SIGNCODE_BUILD
MAKE=$(MAKE) -DSIGNCODE_BUILD
!else
MAKE=$(MAKE)
!endif

#
# To get a .pfx from a .spc and .pvk, run pvk2pfx.exe
#

!IFNDEF SC_PFX_SHA1
SC_PFX_SHA1="$(ROOT)\src\buildtools\certificates\keymantest-sha1.pfx"
!ENDIF

!IFNDEF SC_PFX_SHA256
SC_PFX_SHA256="$(ROOT)\src\buildtools\certificates\keymantest-sha256.pfx"
!ENDIF

!IFNDEF SC_URL
SC_URL="https://keyman.com/"
!ENDIF

!IFNDEF SC_PWD
SC_PWD=""
!ENDIF

SIGNCODE=@$(ROOT)\src\buildtools\signtime.bat signtool.exe $(SC_PFX_SHA1) $(SC_PFX_SHA256) $(SC_URL) $(SC_PWD)

#
# On some computers, the PLATFORM environment variable is set to x86. This can break msbuild
# with our projects. This may be resolvable in the future, but for now the easy fix is ...
# This may be exacerbated by a bug in Delphi MAKE - https://quality.embarcadero.com/browse/RSP-18245
#

PLATFORM=Win32

#
# mkver commands. mkver determines tag from the local build environment variables
# in the same way as /resources/build/build-utils.sh.
#

MKVER_APP=$(PROGRAM)\buildtools\mkver

!IFDEF VERSION_TXT_PATH
MKVER_VERSION_TXT=$(VERSION_TXT_PATH)\version.in
!ELSE
MKVER_VERSION_TXT=..\version.in
!ENDIF

MKVER_TIER_MD=$(KEYMAN_ROOT)\TIER.md
MKVER_VERSION_MD=$(KEYMAN_ROOT)\VERSION.md

MKVER_COMMON_PARAMS=-tier "$(MKVER_TIER_MD)" -version "$(MKVER_VERSION_MD)"

# Update a version.rc file
MKVER_V=$(MKVER_APP) $(MKVER_COMMON_PARAMS) -v $(MKVER_VERSION_TXT) version.in version.rc
# Update a manifest.xml file
MKVER_M=$(MKVER_APP) $(MKVER_COMMON_PARAMS) -m manifest.in manifest.xml
# Token replacement for all other file types; pattern: $(MKVER_U) <f.in> <f.out>
MKVER_U=$(MKVER_APP) $(MKVER_COMMON_PARAMS) -u

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
