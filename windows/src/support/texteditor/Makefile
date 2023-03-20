#
# Keyman Text Editor Makefile
#

!include ..\..\Defines.mak

build:
    $(MSBUILD) editor.vcxproj $(MSBUILD_BUILD)
    $(COPY) Release\editor.exe $(PROGRAM)\support
    $(COPY) Release\editor.pdb $(DEBUGPATH)\support

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) editor.sln
    rmdir /s /q release


signcode:
    $(SIGNCODE) /d "Keyman Engine x64" $(PROGRAM)\support\editor.exe

install:
    $(COPY) $(PROGRAM)\support\editor.exe "$(INSTALLPATH_KEYMANENGINE)"


!include ..\..\Target.mak
# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
