#
# Build all generated source code for standards data
#

!include ..\..\Defines.mak

test: build

STANDARDS_DATA_PATH=$(ROOT)\..\resources\standards-data
STANDARDS_OUT_PATH=$(ROOT)\src\global\delphi\standards

ISO6393_DATA=$(STANDARDS_DATA_PATH)\iso639-3\iso639-3.tab
ISO6393_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.ISO6393ToBCP47Registry.pas

SUBTAG_DATA=$(STANDARDS_DATA_PATH)\language-subtag-registry\language-subtag-registry
SUBTAG_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.BCP47SubtagRegistry.pas

SUPPRESS_DATA=$(STANDARDS_DATA_PATH)\language-subtag-registry\language-subtag-registry
SUPPRESS_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas

LCID_DATA=$(STANDARDS_DATA_PATH)\windows-lcid-to-bcp-47\map_clean_win.txt
LCID_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.LCIDToBCP47Registry.pas

ALLTAGS_DATA=$(STANDARDS_DATA_PATH)\alltags\alltags.txt
ALLTAGS_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.NRSIAllTagsRegistry.pas

LANGTAGS_DATA=$(STANDARDS_DATA_PATH)\langtags\langtags.json
LANGTAGS_PAS=$(STANDARDS_OUT_PATH)\Keyman.System.Standards.LangTagsRegistry.pas

build:
    $(DELPHI_MSBUILD) build_standards_data.dproj
    $(WIN32_TARGET_PATH)\build_standards_data.exe iso6393 $(ISO6393_DATA) $(ISO6393_PAS)
    $(WIN32_TARGET_PATH)\build_standards_data.exe suppress $(SUPPRESS_DATA) $(SUPPRESS_PAS)
    $(WIN32_TARGET_PATH)\build_standards_data.exe subtag $(SUBTAG_DATA) $(SUBTAG_PAS)
    $(WIN32_TARGET_PATH)\build_standards_data.exe lcid $(LCID_DATA) $(LCID_PAS)
    $(WIN32_TARGET_PATH)\build_standards_data.exe alltags $(ALLTAGS_DATA) $(ALLTAGS_PAS)
    $(WIN32_TARGET_PATH)\build_standards_data.exe langtags $(LANGTAGS_DATA) $(LANGTAGS_PAS)

clean: def-clean
    -rd /s/q Win32

!include ..\..\Target.mak
