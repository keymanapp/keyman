#
# Build all generated source code for standards data
#

!include ..\..\Defines.mak

test: build

STANDARDS_DATA_PATH=$(ROOT)\..\resources\standards-data

ISO6393_DATA=$(STANDARDS_DATA_PATH)\iso639-3\iso639-3.tab
SUBTAG_DATA=$(STANDARDS_DATA_PATH)\language-subtag-registry\language-subtag-registry

ISO6393_PAS=$(ROOT)\src\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas
SUBTAG_PAS=$(ROOT)\src\global\delphi\standards\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas

build: 
    $(DELPHI_MSBUILD) build_standards_data.dproj
    Win32\Debug\build_standards_data.exe $(ISO6393_DATA) $(SUBTAG_DATA) $(ISO6393_PAS) $(SUBTAG_PAS)

clean: def-clean
    -rd /s/q Win32

!include ..\..\Target.mak
