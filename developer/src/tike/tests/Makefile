
!include ..\..\Defines.mak

all:
  for %d in (*.kmn) do $(DEVELOPER_PROGRAM)\kmcomp "%d"

.kmn.kmx:
  &$(DEVELOPER_PROGRAM)\kmcomp $**

clean:
  -del *.kmx
