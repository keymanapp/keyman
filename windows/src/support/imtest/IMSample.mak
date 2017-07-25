# Microsoft Developer Studio Generated NMAKE File, Based on IMSample.dsp
!IF "$(CFG)" == ""
CFG=IMSample - Win32 Debug
!MESSAGE No configuration specified. Defaulting to IMSample - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "IMSample - Win32 Release" && "$(CFG)" !=\
 "IMSample - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "IMSample.mak" CFG="IMSample - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "IMSample - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "IMSample - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "IMSample - Win32 Release"

OUTDIR=.
INTDIR=.
# Begin Custom Macros
OutDir=.
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\IMSample.dll"

!ELSE 

ALL : "$(OUTDIR)\IMSample.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\imlib.obj"
	-@erase "$(INTDIR)\imsample.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\IMSample.dll"
	-@erase "$(OUTDIR)\IMSample.exp"
	-@erase "$(OUTDIR)\IMSample.lib"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\IMSample.pch" /YX /FD /c 
CPP_OBJS=.
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IMSample.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\IMSample.pdb" /machine:I386 /def:".\imsample.def"\
 /out:"$(OUTDIR)\IMSample.dll" /implib:"$(OUTDIR)\IMSample.lib" 
DEF_FILE= \
	".\imsample.def"
LINK32_OBJS= \
	"$(INTDIR)\imlib.obj" \
	"$(INTDIR)\imsample.obj"

"$(OUTDIR)\IMSample.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "IMSample - Win32 Debug"

OUTDIR=.
INTDIR=.
# Begin Custom Macros
OutDir=.
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\IMSample.dll"

!ELSE 

ALL : "$(OUTDIR)\IMSample.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\imlib.obj"
	-@erase "$(INTDIR)\imsample.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\IMSample.dll"
	-@erase "$(OUTDIR)\IMSample.exp"
	-@erase "$(OUTDIR)\IMSample.ilk"
	-@erase "$(OUTDIR)\IMSample.lib"
	-@erase "$(OUTDIR)\IMSample.pdb"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)\IMSample.pch" /YX /FD /c 
CPP_OBJS=.
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IMSample.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\IMSample.pdb" /debug /machine:I386 /def:".\imsample.def"\
 /out:"$(OUTDIR)\IMSample.dll" /implib:"$(OUTDIR)\IMSample.lib" /pdbtype:sept 
DEF_FILE= \
	".\imsample.def"
LINK32_OBJS= \
	"$(INTDIR)\imlib.obj" \
	"$(INTDIR)\imsample.obj"

"$(OUTDIR)\IMSample.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "IMSample - Win32 Release" || "$(CFG)" ==\
 "IMSample - Win32 Debug"
SOURCE=.\imlib.cpp
DEP_CPP_IMLIB=\
	".\imlib.h"\
	

"$(INTDIR)\imlib.obj" : $(SOURCE) $(DEP_CPP_IMLIB) "$(INTDIR)"


SOURCE=.\imsample.cpp
DEP_CPP_IMSAM=\
	".\imlib.h"\
	

"$(INTDIR)\imsample.obj" : $(SOURCE) $(DEP_CPP_IMSAM) "$(INTDIR)"



!ENDIF 

