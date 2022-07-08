#
# Keyman Developer Delphi components Makefile
#

!include ..\..\..\Defines.mak

# ----------------------------------------------------------------------

all: keyman-developer-components

build: keyman-developer-components

# ----------------------------------------------------------------------

keyman-developer-components: dirs compile install
#   $(COPY) keyman_components.bpl $(OUTLIB)

compile:
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(DEVELOPER_DCC32DPK) developer_components.dpk

install:
    $(DEVTOOLS) -ip $(DEVELOPER_OUTLIB)\developer_components.bpl

signcode:
  @rem no signcode

symbols:
  @rem no symbols

# ----------------------------------------------------------------------
# clean target
# ----------------------------------------------------------------------

clean: def-clean
    -rd /s/q obj

# ----------------------------------------------------------------------

!include ..\..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
