#
#
#
!include ..\..\defines.mak

build: mkver-app
  cd $(ROOT)\src\global\help
  $(MKVER_U) version.php.in version.out

clean:
  -del version.out

!include ..\..\target.mak
