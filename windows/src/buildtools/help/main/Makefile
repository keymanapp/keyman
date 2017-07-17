
!include ..\Defines.mak

all:	contents.html \
	website.html \
        prereq.html \
        build.html \
        testing.html \
        upload.html \
        stock.html

clean:
  -del /Q *.html

.php.html:
  $(PHP) $< > $@
