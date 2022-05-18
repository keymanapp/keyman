!IFNDEF KEYMAN_ROOT
!error KEYMAN_ROOT must be defined
!ENDIF

DEVELOPER_ROOT=$(KEYMAN_ROOT)\developer
WINDOWS_ROOT=$(KEYMAN_ROOT)\windows

DEVELOPER_PROGRAM=$(DEVELOPER_ROOT)\bin
DEVELOPER_DEBUGPATH=$(DEVELOPER_ROOT)\debug

# Temporary import of windows/src/Defines.mak
!include $(WINDOWS_ROOT)\src\Defines.mak
