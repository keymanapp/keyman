
!include ..\Defines.mak

build: global-versions
  cd $(ROOT)\src\global\delphi
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\help
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\inst
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\wix
  $(MAKE) $(TARGET)

global-versions:
  $(MKVER_U) $(ROOT)\src\global\inc\keymanversion_build.in $(ROOT)\src\global\inc\keymanversion_build.h $(MKVER_VERSION_TXT)
  $(MKVER_U) $(ROOT)\src\global\delphi\general\keymanversion_build.in $(ROOT)\src\global\delphi\general\keymanversion_build.inc $(MKVER_VERSION_TXT)
  
build-release:
  @rem

clean:
  -del $(ROOT)\src\global\delphi\general\keymanversion_build.inc
  -del $(ROOT)\src\global\inc\keymanversion_build.h
  
  cd $(ROOT)\src\global\delphi
  $(MAKE) clean

  cd $(ROOT)\src\global\help
  $(MAKE) clean

  cd $(ROOT)\src\global\inst
  $(MAKE) clean

  cd $(ROOT)\src\global\wix
  $(MAKE) clean

signcode:
  @rem no signcode

backup:
  @rem
  
test-manifest:
  @rem
  
install:
  cd $(ROOT)\src\global\delphi
  $(MAKE) install

!include ..\target.mak