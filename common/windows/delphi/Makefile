!include Defines.mak

global-versions:
# Build our root version.txt
    cd $(KEYMAN_ROOT)\common\windows

    $(MKVER_U) $(KEYMAN_ROOT)\common\windows\cpp\include\keymanversion_build.in $(KEYMAN_ROOT)\common\windows\cpp\include\keymanversion_build.h
    $(MKVER_U) $(KEYMAN_ROOT)\common\windows\delphi\general\keymanversion_build.in $(KEYMAN_ROOT)\common\windows\delphi\general\keymanversion_build.inc

!include Target.mak