#
# Install tools Makefile
#

!include ..\..\Defines.mak

build: copydebug

clean:
    cd $(ROOT)\src\buildtools\inst
    -del copydebug.mak
    -del copytointernalftp.mak
    -del srcbackup.mak

copydebug:
    cd $(ROOT)\src
    -del $(ROOT)\src\tds_file.txt
    dir /b/s *.dbg *.pdb > $(ROOT)\src\tds_file.txt
    $(WZZIP) -x!*test\* -x!vc*.pdb tds.zip @tds_file.txt
    -del $(ROOT)\src\tds_file.txt

    cd $(ROOT)\src\buildtools\inst
    $(PROGRAM)\buildtools\mkver -v -u copydebug.in copydebug.mak $(ROOT)\src\developer\version.txt
    $(MAKE) -fcopydebug.mak

uploadsymbols:
    cd $(ROOT)\src\buildtools\inst
    $(PROGRAM)\buildtools\mkver -v -u copydebug.in copydebug.mak $(ROOT)\src\developer\version.txt
    $(MAKE) -fcopydebug.mak uploadsymbols

srcbackup:
    cd $(ROOT)\src\buildtools\inst
    $(PROGRAM)\buildtools\mkver -v -u $(ROOT)\src\buildtools\inst\srcbackup.in srcbackup.mak $(ROOT)\src\developer\version.txt
    make -fsrcbackup.mak

copytointernalftp:
    cd $(ROOT)\src\buildtools\inst
    $(PROGRAM)\buildtools\mkver -v -u $(ROOT)\src\buildtools\inst\copytointernalftp.in copytointernalftp.mak $(ROOT)\src\developer\version.txt
    make -fcopytointernalftp.mak

!include ..\..\Target.mak
