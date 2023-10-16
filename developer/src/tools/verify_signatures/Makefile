#
# test for signatures and version information being correct in bin folder
#

!include ..\..\Defines.mak

test: prereq
  $(SIGCHECK) $(DEVELOPER_PROGRAM)\* > sig1
  $(VERIFY_SIGNATURES) < sig1

# prereq may not be needed?
prereq:
  cd $(VERIFY_SIGNATURES_PATH)
  $(MAKE)

clean: def-clean
  -del sig1

!include ..\..\Target.mak
