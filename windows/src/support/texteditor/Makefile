#
# Keyman Text Editor Makefile
#

!include ..\..\Defines.mak

build:
    $(MSBUILD) editor.vcxproj $(MSBUILD_BUILD) /p:Platform=x86
		$(MSBUILD) editor.vcxproj $(MSBUILD_BUILD) /p:Platform=x64
    $(COPY) $(WIN32_TARGET_PATH)\editor32.exe $(PROGRAM)\support
    $(COPY) $(WIN32_TARGET_PATH)\editor32.pdb $(DEBUGPATH)\support
		$(COPY) $(X64_TARGET_PATH)\editor64.exe $(PROGRAM)\support
    $(COPY) $(X64_TARGET_PATH)\editor64.pdb $(DEBUGPATH)\support

clean: def-clean
    $(MSBUILD) $(MSBUILD_CLEAN) editor.sln

signcode:
    $(SIGNCODE) /d "Text Editor for testing Keyman" $(PROGRAM)\support\editor32.exe
		$(SIGNCODE) /d "Text Editor for testing Keyman" $(PROGRAM)\support\editor64.exe

install:
    $(COPY) $(PROGRAM)\support\editor32.exe "$(INSTALLPATH_KEYMANENGINE)"
		$(COPY) $(PROGRAM)\support\editor64.exe "$(INSTALLPATH_KEYMANENGINE)"

wrap-symbols:
		$(SYMSTORE) $(PROGRAM)\support\editor32.exe /t keyman-windows
		$(SYMSTORE) $(PROGRAM)\support\editor64.exe /t keyman-windows
		$(SYMSTORE) $(DEBUGPATH)\support\editor32.pdb /t keyman-windows
		$(SYMSTORE) $(DEBUGPATH)\support\editor64.pdb /t keyman-windows


!include ..\..\Target.mak
# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
