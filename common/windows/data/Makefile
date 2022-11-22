!include ..\delphi\Defines.mak

BUILDUNIDATA_PATH=$(KEYMAN_ROOT)\common\windows\delphi\tools\buildunidata
BUILDUNIDATA_PROGRAM=$(BUILDUNIDATA_PATH)\$(WIN32_TARGET_PATH)\buildunidata.exe

DATA_SOURCE=$(KEYMAN_ROOT)\resources\standards-data\unicode-character-database
DATABASE_PATH=$(KEYMAN_ROOT)\common\windows\bin\data
DATABASE_FILENAME=$(DATABASE_PATH)\unicodedata.mdb

build: buildunidata
  -mkdir "$(DATABASE_PATH)"
# cd "$(KEYMAN_ROOT)\common\windows\data"
  "$(BUILDUNIDATA_PROGRAM)" "$(DATA_SOURCE)" "$(DATABASE_FILENAME)"

clean:
  -del "$(DATABASE_FILENAME)"

buildunidata:
  cd "$(BUILDUNIDATA_PATH)"
  $(MAKE) build
