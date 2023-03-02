#
# Keyman64-ver Makefile
#



!include ..\..\Defines.mak

build: version.res dirs
   $(MKVER_U) download.in download.mak
	 $(MAKE) -fdownload.mak build

#   $(COPY) keyman64-ver.lib $(OUTLIB) - already done

clean: def-clean
		$(MKVER_U) download.in download.mak
    $(MAKE) -fdownload.mak clean

signcode:
    $(MKVER_U) download.in download.mak
    $(MAKE) -fdownload.mak signcode

wrap-symbols:
    $(MKVER_U) download.in download.mak
    $(MAKE) -fdownload.mak wrap-symbols

install:
    $(MKVER_U) download.in download.mak
    $(MAKE) -fdownload.mak install

!include ..\..\Target.mak
