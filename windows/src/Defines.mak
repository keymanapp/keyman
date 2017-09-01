# DEBUG=1

!IFDEF KEYMAN_ROOT
ROOT=$(KEYMAN_ROOT)\windows
!ELSE
ROOT=c:\keyman\windows
!ENDIF

!IFDEF KEYMAN_ENCUMBERED_ROOT
ENCUMBERED_EXT=$(KEYMAN_ENCUMBERED_ROOT)\src\ext
!ELSE
ENCUMBERED_EXT=c:\keyman_extra\windows\encumbered_src\ext
!ENDIF

EXT=$(ROOT)\src\ext

INCLUDE=$(ROOT)\src\global\inc;$(INCLUDE)

!ifndef EXCLUDEPATHDEFINES
!include $(ROOT)\src\PathDefines.mak
!endif

PROGRAM=$(ROOT)\bin
BUILD=$(ROOT)\build
DEBUGPATH=$(ROOT)\debug
OUTLIB=$(ROOT)\lib

!IFDEF DEBUG
  MAKEFLAG_DEBUG=-DDEBUG
!ENDIF

!IFDEF USE_PLUSMEMO
  MAKEFLAG_USE_PLUSMEMO=-DUSE_PLUSMEMO
!ENDIF

!IFDEF DELPHI_STARTER
!IFDEF USE_PLUSMEMO
!ERROR DELPHI_STARTER and USE_PLUSMEMO cannot be used together
!ENDIF

  MAKEFLAG_DELPHI_STARTER=-DDELPHI_STARTER
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

MAKE=$(MAKE) -l $(MAKEFLAG_USE_PLUSMEMO) $(MAKEFLAG_USERDEFINES) $(MAKEFLAG_DEBUG) $(MAKEFLAG_BUILDHELP) $(MAKEFLAG_BUILDRTF) $(MAKEFLAG_SC_TIMESTAMP) $(MAKEFLAG_LINT) $(MAKEFLAG_QUIET) $(MAKEFLAG_DELPHI_STARTER)

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

!IFDEF DELPHI_STARTER
DCC32=@$(DEVTOOLS) -dccx
DCC32DPK=@$(DEVTOOLS) -dccx
!ELSE
!IFDEF NOUI
DCC32=dcc32.exe $(DELPHIDPRPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
DCC32DPK=dcc32.exe $(DELPHIDPKPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
!ELSE
!IFDEF QUIET
DCC32=@$(DEVTOOLS) -dccq  $(DELPHIDPRPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
DCC32DPK=@$(DEVTOOLS) -dccq $(DELPHIDPKPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
!ELSE
DCC32=@$(DEVTOOLS) -dcc $(DELPHIDPRPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
DCC32DPK=@$(DEVTOOLS) -dcc $(DELPHIDPKPARAMS) $(MAKEFLAG_USE_PLUSMEMO)
!ENDIF
!ENDIF
!ENDIF

DCC64=dcc64.exe $(DELPHIDPRPARAMS64) -N0x64\ -Ex64\

#
# Other program build commands
#

PHPDIR=\php
PHPEXE=$(PHPDIR)\php.exe

BRCC32=rc.exe

HHC=\progra~1\htmlhe~1\hhc
NMAKE=nmake.exe
CL=cl.exe 
MSBUILD=msbuild.exe /maxcpucount
MT="C:\Program Files (x86)\Windows Kits\8.0\bin\x86\mt.exe"
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
WZZIP="C:\program files\7-zip\7z.exe" a
WZUNZIP="C:\program files\7-zip\7z.exe" e

XSLTPROC=$(ROOT)\src\ext\libxslt\xsltproc.exe

# TDSPACK=error! $(ROOT)\src\buildtools\tdspack\tdspack -e
# TDSPACKCOMPRESS=error! $(ROOT)\src\buildtools\tdspack\tdspack -o

!IFDEF DELPHI_STARTER
#
# The Delphi Starter build does not generate .tds files for projects
# by default so we just ignore this. We don't really need to have
# the .dbg files for most developers anyway, just for release builds
#
TDS2DBG=rem
!ELSE
TDS2DBG=$(ROOT)\bin\buildtools\tds2dbg
!ENDIF

MAKEJCLDBG=$(ROOT)\bin\buildtools\makejcldbg.exe -E

WIXPATH="c:\program files (x86)\windows installer xml v3.5\bin"
WIXCANDLE=$(WIXPATH)\candle.exe -wx

!IFDEF LINT
WIXLIGHT=$(WIXPATH)\light.exe -wx
!ELSE
# we suppress ICE82 because it reports spurious errors with merge module keymanengine to do with duplicate sequence numbers.  Safely ignored.
WIXLIGHT=$(WIXPATH)\light.exe -wx -sice:ICE82 -sice:ICE80
!ENDIF

WIXLIT=$(WIXPATH)\lit.exe -wx

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

SC_URL="http://www.keyman.com/"
SC_PFX_SHA256="$(ROOT)\src\buildtools\certificates\keymantest-sha256.pfx"
SC_PFX_SHA1="$(ROOT)\src\buildtools\certificates\keymantest-sha1.pfx"
SC_PWD=""

SIGNCODE=@$(ROOT)\src\buildtools\signtime.bat signtool.exe $(SC_PFX_SHA1) $(SC_PFX_SHA256) $(SC_URL) $(SC_PWD)

!ifdef USERDEFINES
!include $(ROOT)\src\UserDefines.mak
!endif

#
# On some computers, the PLATFORM environment variable is set to x86. This can break msbuild
# with our projects. This may be resolvable in the future, but for now the easy fix is ...
# This may be exacerbated by a bug in Delphi MAKE - https://quality.embarcadero.com/browse/RSP-18245
#

PLATFORM=Win32
