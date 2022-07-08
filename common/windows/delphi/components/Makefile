#
# Common Delphi components Makefile
#

!include ..\Defines.mak

# ----------------------------------------------------------------------

all: common-components

build: common-components

# ----------------------------------------------------------------------

common-components: compile install
#   $(COPY) keyman_components.bpl $(OUTLIB)

compile:
    @-mkdir obj\Win32\$(TARGET_PATH)
    $(COMMON_DCC32DPK) common_components.dpk

install:
    $(DEVTOOLS) -ip $(COMMON_OUTLIB)\common_components.bpl

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

!include ..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
