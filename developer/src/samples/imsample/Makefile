#
# imsample Makefile
#

VERSION_TXT_PATH=..\..

!include ..\..\Defines.mak

build: dirs version.res
    $(MSBUILD) imsample.sln $(MSBUILD_BUILD) "/p:Platform=Win32"
    $(COPY) $(WIN32_TARGET_PATH)\imsample.dll $(DEVELOPER_PROGRAM)
    $(COPY) $(WIN32_TARGET_PATH)\imsample.pdb $(DEVELOPER_PROGRAM)

		$(MSBUILD) imsample.sln $(MSBUILD_BUILD) "/p:Platform=x64"
    $(COPY) $(X64_TARGET_PATH)\imsample.x64.dll $(DEVELOPER_PROGRAM)
    $(COPY) $(X64_TARGET_PATH)\imsample.x64.pdb $(DEVELOPER_PROGRAM)


signcode:
    $(SIGNCODE) /d "Keyman IMX Sample" $(DEVELOPER_PROGRAM)\imsample.dll
		$(SIGNCODE) /d "Keyman IMX Sample" $(DEVELOPER_PROGRAM)\imsample.x64.dll


wrap-symbols:
    $(SYMSTORE) $(DEVELOPER_PROGRAM)\imsample.dll /t keyman-developer
		$(SYMSTORE) $(DEVELOPER_PROGRAM)\imsample.x64.dll /t keyman-developer
    $(SYMSTORE) $(DEVELOPER_DEBUGPATH)\imsample.pdb /t keyman-developer
		$(SYMSTORE) $(DEVELOPER_DEBUGPATH)\imsample.x64.pdb /t keyman-developer

clean: def-clean
    $(MSBUILD) imsample.sln $(MSBUILD_CLEAN)

install:
    @rem

!include ..\..\Target.mak
