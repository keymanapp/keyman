
!include ..\..\Defines.mak

all:
  for %d in (*.kmn) do $(DEVELOPER_PROGRAM)\kmc.cmd build "%d"

.kmn.kmx:
  &$(DEVELOPER_PROGRAM)\kmc.cmd build $**

clean:
  -del *.kmx
