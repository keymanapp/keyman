#
# test for signatures and version information being correct in bin folder
#

!include ..\..\Defines.mak

test: prereq
  $(SIGCHECK) $(ROOT)\bin\desktop\* > sig1
  $(SIGCHECK) $(ROOT)\bin\engine\* >> sig1
  $(SIGCHECK) $(ROOT)\bin\inst\* >> sig1
  $(VERIFY_SIGNATURES) < sig1

# prereq may not be needed?
prereq:
  cd $(VERIFY_SIGNATURES)
  $(MAKE)

clean: def-clean
  -del sig1

!include ..\..\Target.mak
