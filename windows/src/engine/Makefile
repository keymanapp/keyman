#
# Keyman Engine Makefile
#

# ----------------------------------------------------------------------

!ifdef NODELPHI
TARGETS=keymanmc keyman32 keyman32-ver keyman64 keyman64-ver keymanx64 mcompile kmtip kmrefresh
!else
TARGETS=keymanmc keyman32 keyman32-ver kmcomapi keyman tsysinfox64 tsysinfo keyman64 keyman64-ver keymanx64 mcompile inst kmtip kmrefresh
!endif
MANIFESTS=keyman tsysinfo tsysinfox64 keymanx64 mcompile
CLEANS=clean-engine

!include ..\Header.mak

# ----------------------------------------------------------------------

keyman: redist
    cd $(ROOT)\src\engine\keyman
    $(MAKE) $(TARGET)

kmcomapi: .virtual
    cd $(ROOT)\src\engine\kmcomapi
    $(MAKE) $(TARGET)

keymanmc: .virtual
    cd $(ROOT)\src\engine\keymanmc
    $(MAKE) $(TARGET)

keyman32: .virtual
    cd $(ROOT)\src\engine\keyman32
    $(MAKE) $(TARGET)

keyman32-ver: .virtual
    cd $(ROOT)\src\engine\keyman32-ver
    $(MAKE) $(TARGET)

tsysinfox64: .virtual
    cd $(ROOT)\src\engine\tsysinfox64
    $(MAKE) $(TARGET)

tsysinfo: tsysinfox64
    cd $(ROOT)\src\engine\tsysinfo
    $(MAKE) $(TARGET)

keymanx64: .virtual
    cd $(ROOT)\src\engine\keymanx64
    $(MAKE) $(TARGET)

keyman64: .virtual
    cd $(ROOT)\src\engine\keyman64
    $(MAKE) $(TARGET)

keyman64-ver: .virtual
    cd $(ROOT)\src\engine\keyman64-ver
    $(MAKE) $(TARGET)

kmrefresh: .virtual
    cd $(ROOT)\src\engine\kmrefresh
    $(MAKE) $(TARGET)

mcompile: .virtual
    cd $(ROOT)\src\engine\mcompile
    $(MAKE) $(TARGET)

kmtip: .virtual
    cd $(ROOT)\src\engine\kmtip
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------
# Shorthand targets for debug builds
# ----------------------------------------------------------------------

keyman32-ver-build:
    cd $(ROOT)\src\engine\keyman32-ver
    $(MAKE) build

keyman32-ver-signcode:
    cd $(ROOT)\src\engine\keyman32-ver
    $(MAKE) signcode

keyman32-ver-install:
    cd $(ROOT)\src\engine\keyman32-ver
    $(MAKE) install

keyman64-ver-build:
    cd $(ROOT)\src\engine\keyman64-ver
    $(MAKE) build

keyman64-ver-signcode:
    cd $(ROOT)\src\engine\keyman64-ver
    $(MAKE) signcode

keyman64-ver-install:
    cd $(ROOT)\src\engine\keyman64-ver
    $(MAKE) install

# ----------------------------------------------------------------------

redist:
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\sentry.dll $(ROOT)\bin\engine
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\sentry.x64.dll $(ROOT)\bin\engine
    copy $(KEYMAN_ROOT)\common\windows\delphi\ext\sentry\crashpad_handler.exe $(ROOT)\bin\engine

# ----------------------------------------------------------------------

inst: redist
    cd $(ROOT)\src\engine\inst
    $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

clean-engine:
    cd $(ROOT)\src\engine
    -del version.txt

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
