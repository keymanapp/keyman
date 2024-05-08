!IFNDEF KEYMAN_ROOT
!error KEYMAN_ROOT must be defined
!ENDIF

WINDOWS_ROOT=$(KEYMAN_ROOT)\windows
WINDOWS_PROGRAM=$(WINDOWS_ROOT)\bin

COMMON_ROOT=$(KEYMAN_ROOT)\common\windows\delphi

DEVELOPER_ROOT=$(KEYMAN_ROOT)\developer
DEVELOPER_PROGRAM=$(DEVELOPER_ROOT)\bin
DEVELOPER_OUTLIB=$(DEVELOPER_ROOT)\lib
DEVELOPER_DEBUGPATH=$(DEVELOPER_ROOT)\debug

DEVELOPER_DELPHIDPKPARAMS=-Q -B -GD -VT -^$C+ -^$D+ -^$J+ -^$L+ -^$O+ -^$Q- -^$R- -^$W+ -^$Y+ -E. $(DELPHIWARNINGS) -I$(DELPHIINCLUDES) -U$(DELPHIINCLUDES) -R$(DELPHIINCLUDES) -NSVcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Web;Soap;Winapi;System.Win -LE$(DEVELOPER_OUTLIB) -LN$(DEVELOPER_OUTLIB) -NSData -NUobj\Win32\$(TARGET_PATH)
DEVELOPER_DCC32DPK=cmd /c "$(DCC32PATH)\dcc32.exe" $(DEVELOPER_DELPHIDPKPARAMS)

# Temporary import of windows/src/Defines.mak
# TODO: include COMMON_ROOT's defines.mak instead
!include $(WINDOWS_ROOT)\src\Defines.mak

KEYMANCORE=keymancore-2
