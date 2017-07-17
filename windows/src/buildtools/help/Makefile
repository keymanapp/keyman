#-------------------------------------------------------
# Makefile for building help file
#-------------------------------------------------------

!include Defines.mak
!include ..\..\Defines.mak

#-------------------------------------------------------
# Main targets
#-------------------------------------------------------

build: helpfile dirs
  copy /Y kmbuild.chm $(ROOT)\bin\buildtools
  copy /Y kmbuild.bat $(ROOT)\bin\buildtools

open: build
  start $(ROOT\bin\buildtools\kmbuild.chm

#-------------------------------------------------------
# Build targets
#-------------------------------------------------------

helpfile: inc idx main
  -$(HHC) kmbuild.hhp

inc:
  cd inc
  $(MAKE) all
  cd ..

idx:
  cd idx
  $(MAKE) all
  cd ..

main:
  cd main
  $(MAKE) all
  cd ..

#-------------------------------------------------------
# Clean targets
#-------------------------------------------------------

clean: dirs-clean
  -del /Q kmbuild.chm

dirs-clean: inc-clean main-clean idx-clean

inc-clean:
  cd inc
  $(MAKE) clean
  cd ..

idx-clean:
  cd idx
  $(MAKE) clean
  cd ..

main-clean:
  cd main
  $(MAKE) clean
  cd ..

!include ..\..\Target.mak

#-------------------------------------------------------
# EOF
#-------------------------------------------------------
