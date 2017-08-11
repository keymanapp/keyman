
!include ..\..\..\Defines.mak

all:
  for %d in (*.kmn) do $(PROGRAM)\dev\kmcomp "%d"

.kmn.kmx:
  &$(PROGRAM)\dev\kmcomp $**

clean:
  -del *.kmx
