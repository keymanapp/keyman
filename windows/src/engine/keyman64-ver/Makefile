#
# Keyman64-ver Makefile
#



!include ..\..\Defines.mak

build: version.res dirs
   $(MKVER_U) premake.in premake.mak
	 $(MKVER_U) $(ROOT)\src\engine\keyman32-ver\keyman64-build-version.in $(ROOT)\src\engine\keyman32-ver\keyman64-build-version.h
	 $(MAKE) -fpremake.mak build

#   $(COPY) keyman64-ver.lib $(OUTLIB) - already done

clean: def-clean premake.mak
    $(MAKE) -fpremake.mak clean

signcode: premake.mak
    $(MAKE) -fpremake.mak signcode

wrap-symbols: premake.mak
    $(MAKE) -fpremake.mak wrap-symbols

install: premake.mak
    $(MAKE) -fpremake.mak install
    
premake.mak:
    $(MKVER_U) premake.in premake.mak

!include ..\..\Target.mak
