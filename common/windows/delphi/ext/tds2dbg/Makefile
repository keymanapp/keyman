info:
  echo The following builds require Borland C++ compiler to be installed and in the path; see Readme.txt for details.
  echo So, the tds2dbg.bin is kept in the repo in order to make builds simpler

BCC55DIR=c:\borland\bcc55

tds2dbg.exe: main.cpp tds2dbg.cpp fileutils.cpp log.cpp
# $(CL) /Fe$< /EHsc $**
  path $(BCC55DIR)\bin;$(PATH)
  $(BCC55DIR)\bin\bcc32 -y -v -L$(BCC55DIR)\Lib -I$(BCC55DIR)\Include -e$< $**

test.exe: testa.cpp
# $(CL) /Fe$< /EHsc $**
  $(BCC55DIR)\bin\bcc32 -y -v -M -L$(BCC55DIR)\Lib -I$(BCC55DIR)\Include -e$< $**

clean:
  -del /Q tds2dbg.obj fileutils.obj log.obj main.obj tds2dbg.tds
  -del /Q testa.obj test.exe test.tds test.dbg
  -rmdir /S /Q html

doc:
  doxygen tds2dbg.dcf
