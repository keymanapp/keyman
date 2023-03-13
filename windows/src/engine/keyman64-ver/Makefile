#
# Keyman64-ver Makefile
#



!include ..\..\Defines.mak

build: version.res dirs
   $(MKVER_U) premake.in premake.mak
	 $(MAKE) -fpremake.mak build

#   $(COPY) keyman64-ver.lib $(OUTLIB) - already done

clean: def-clean
		$(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak clean

signcode:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak signcode

wrap-symbols:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak wrap-symbols

install:
    $(MKVER_U) premake.in premake.mak
    $(MAKE) -fpremake.mak install

!include ..\..\Target.mak
