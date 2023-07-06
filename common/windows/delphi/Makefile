!include Defines.mak

global-versions:
# Build our root version.txt; TODO: replace with build.sh deps
    cd $(KEYMAN_ROOT)\common\windows
    $(MKVER_U) $(KEYMAN_ROOT)\common\windows\delphi\general\keymanversion_build.in $(KEYMAN_ROOT)\common\windows\delphi\general\keymanversion_build.inc
    $(GIT_BASH_FOR_KEYMAN) $(KEYMAN_ROOT)\common\include\build.sh clean configure build

!include Target.mak
