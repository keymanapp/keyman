
!include ..\..\..\Defines.mak

all:
  for %d in (*.kmn) do $(PROGRAM)\bin\kmc.cmd build "%d"

.kmn.kmx:
  &$(PROGRAM)\bin\kmc.cmd build $**

clean:
  -del *.kmx
