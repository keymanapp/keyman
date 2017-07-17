
!include ..\Defines.mak

all:	toc.hhc \
	index.hhk

clean:
  -del /Q toc.hhc
  -del /Q index.hhk

.php.html:
  $(PHP) $< > $@

.php.hhc:
  $(PHP) $< > $@

.php.hhk:
  $(PHP) $< > $@
