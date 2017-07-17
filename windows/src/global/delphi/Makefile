#
# Keyman root Makefile
#

!include ..\..\Defines.mak

# ----------------------------------------------------------------------

all: keyman-components

# ----------------------------------------------------------------------

keyman-components: dirs compile install
    # $(COPY) keyman_components.bpl $(OUTLIB)

compile:
    $(DCC32DPK) keyman_components.dpk

install:
    $(DEVTOOLS) -ip $(OUTLIB)\keyman_components.bpl

# ----------------------------------------------------------------------
# clean target
# ----------------------------------------------------------------------

clean: clean-keyman-components

clean-keyman-components: def-clean
    $(CLEAN) *.bpl

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
