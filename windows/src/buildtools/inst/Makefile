#
# Install tools Makefile
#

!include ..\..\Defines.mak

build: copydebug

clean:
    cd $(ROOT)\src\buildtools\inst
    -del copydebug.mak
    -del srcbackup.mak

copydebug:
    cd $(ROOT)\src
    -del $(ROOT)\src\tds_file.txt
    dir /b/s desktop\*.dbg desktop\*.pdb > $(ROOT)\src\tds_file.txt
    dir /b/s developer\*.dbg developer\*.pdb >> $(ROOT)\src\tds_file.txt
    dir /b/s engine\*.dbg engine\*.pdb >> $(ROOT)\src\tds_file.txt
    $(WZZIP) -x!vc*.pdb tds.zip @tds_file.txt
    -del $(ROOT)\src\tds_file.txt

    cd $(ROOT)\src\buildtools\inst
    $(MKVER_U) copydebug.in copydebug.mak
    $(MAKE) -fcopydebug.mak

uploadsymbols:
    cd $(ROOT)\src\buildtools\inst
    $(MKVER_U) copydebug.in copydebug.mak
    $(MAKE) -fcopydebug.mak uploadsymbols

srcbackup:
    cd $(ROOT)\src\buildtools\inst
    $(MKVER_U) $(ROOT)\src\buildtools\inst\srcbackup.in srcbackup.mak
    make -fsrcbackup.mak

!include ..\..\Target.mak
