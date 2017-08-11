
!include ..\Defines.mak

build:
  cd $(ROOT)\src\global\delphi
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\help
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\inst
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\wix
  $(MAKE) $(TARGET)

build-release:
  @rem

clean:
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