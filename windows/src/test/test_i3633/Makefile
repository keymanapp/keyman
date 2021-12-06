#
# test for signatures and version information being correct in ?install directory or ?bin directory
#

!include ..\..\Defines.mak

SIGCHECK=-$(WIN32_TARGET_PATH)\sigcheck -q -s -e -v -accepteula
VERIFY=$(WIN32_TARGET_PATH)\verify -d $(KEYMAN_ROOT)\VERSION.md

test: test-bin

test-bin: prereq
  $(SIGCHECK) $(ROOT)\bin\desktop\* > sig1
  $(SIGCHECK) $(ROOT)\bin\developer\* >> sig1
  $(SIGCHECK) $(ROOT)\bin\engine\* >> sig1
  $(SIGCHECK) $(ROOT)\bin\inst\* >> sig1
  $(VERIFY) < sig1

test-install: prereq


prereq:
  $(DELPHI_MSBUILD) verify.dproj "/p:Platform=Win32"
  copy sigcheck.bin $(WIN32_TARGET_PATH)\sigcheck.exe


clean: def-clean
  -del sig1
  -del sig2

!include ..\..\Target.mak
