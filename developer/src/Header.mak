!IFNDEF KEYMAN_ROOT
!error KEYMAN_ROOT must be defined
!ENDIF

DEVELOPER_ROOT=$(KEYMAN_ROOT)\developer
WINDOWS_ROOT=$(KEYMAN_ROOT)\windows

# Temporary import of windows/src/Header.mak
!include $(WINDOWS_ROOT)\src\Header.mak
