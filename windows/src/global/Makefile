
!include ..\Defines.mak

build:
!ifdef NODELPHI
  echo Skipping global
!else
  cd $(KEYMAN_ROOT)\common\windows\delphi\components
  $(MAKE) $(TARGET)

  cd $(ROOT)\src\global\delphi
  $(MAKE) $(TARGET)

  cd $(KEYMAN_ROOT)\common\windows\data
  $(MAKE) $(TARGET)
!endif

build-release:
  @rem

clean:
  -del $(ROOT)\src\global\delphi\general\keymanversion_build.inc
  -del $(KEYMAN_ROOT)\common\windows\cpp\include\keymanversion_build.h

  cd $(ROOT)\src\global\delphi
  $(MAKE) clean

  cd $(KEYMAN_ROOT)\common\windows\data
  $(MAKE) clean

signcode:
  @rem no signcode

test-manifest:
  @rem

wrap-symbols:
  @rem

install:
  cd $(ROOT)\src\global\delphi
  $(MAKE) install

!include ..\target.mak