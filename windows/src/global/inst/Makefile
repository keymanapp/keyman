!include ..\..\Defines.mak


build: buildunidata
  cd $(ROOT)\src\global\inst\data
  $(ROOT)\bin\buildtools\buildunidata . .\UnicodeData.mdb

clean:
  cd $(ROOT)\src\global\inst
  del data\unicodedata.mdb  

buildunidata:
  cd $(ROOT)\src\buildtools\buildunidata
  $(MAKE) build
