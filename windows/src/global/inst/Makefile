!include ..\..\Defines.mak

BUILDUNIDATA_PATH=$(KEYMAN_ROOT)\common\windows\delphi\tools\buildunidata
BUILDUNIDATA_PROGRAM=$(BUILDUNIDATA_PATH)\$(WIN32_TARGET_PATH)\buildunidata.exe

build: buildunidata
  cd $(ROOT)\src\global\inst\data
  $(BUILDUNIDATA_PROGRAM) $(KEYMAN_ROOT)\resources\standards-data\unicode-character-database .\UnicodeData.mdb

clean:
  cd $(ROOT)\src\global\inst
  del data\unicodedata.mdb

buildunidata:
  cd $(BUILDUNIDATA_PATH)
  $(MAKE) build
