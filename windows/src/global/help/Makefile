#
#
#
!include ..\..\defines.mak

build: mkver-app
  cd $(ROOT)\src\global\help
  $(PROGRAM)\buildtools\mkver -v -u version.in version.out $(ROOT)\src\developer\version.txt

clean:
  -del version.out

!include ..\..\target.mak
