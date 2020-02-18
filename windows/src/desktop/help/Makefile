#-------------------------------------------------------
# Makefile for building help file
#-------------------------------------------------------

!include ..\..\Defines.mak

#-------------------------------------------------------
# Style Sheets
#-------------------------------------------------------

XSLSTYLE_CHM=chm.xsl
XSLSTYLE_HTML=html.xsl
XSLSTYLE_BIGHTML=bightml.xsl
XSLSTYLE_PHP=php.xsl

#-------------------------------------------------------
# Main targets
#-------------------------------------------------------

build:
    @rem

build-release: chm html php
    copy /Y keymandesktop.chm $(ROOT)\bin\desktop

pdfdocs:
    @rem

signcode:
    @rem

backup:
    @rem

clean:
	-del *.htm
	-del alias.h
	-del context.h
	-del keymandesktop.chm
	-del keymandesktop.hhp
	-del keymandesktop.log
	-del toc.hhc
  -del help.suo

#-------------------------------------------------------
# CHM build
#-------------------------------------------------------

chm:  keymandesktop.chm

keymandesktop.hhp:  index.htm

.xml.htm:
  $(XSLTPROC) --xinclude --timing --output $@ $(XSLSTYLE_CHM) $<

.hhp.chm:
	-1 "c:\program files (x86)\html help workshop\hhc" $<

#-------------------------------------------------------
# HTML build
#-------------------------------------------------------

html: html_cleanbin html_images
  $(XSLTPROC) --xinclude --timing $(XSLSTYLE_HTML) index.xml

html_cleanbin:
  -rd /s/q $(ROOT)\bin\help\desktop
  $(MAKE) dirs

html_images: dirs
  -rd /s/q $(ROOT)\bin\help\desktop\desktop_images
  xcopy /s/q $(ROOT)\src\desktop\help\desktop_images $(ROOT)\bin\help\desktop\desktop_images\ #
  copy $(ROOT)\src\desktop\help\kmhelp.css $(ROOT)\bin\help\desktop

#-------------------------------------------------------
# PHP build
#-------------------------------------------------------

php: php_cleanbin php_images
  $(XSLTPROC) --xinclude --timing $(XSLSTYLE_PHP) index.xml

php_cleanbin:
  -rd /s/q $(ROOT)\bin\help\php\desktop
  $(MAKE) dirs

php_images: dirs
  -rd /s/q $(ROOT)\bin\help\php\desktop\desktop_images
  xcopy /s/q $(ROOT)\src\desktop\help\desktop_images $(ROOT)\bin\help\php\desktop\desktop_images\ #
  copy $(ROOT)\src\desktop\help\kmhelp.css $(ROOT)\bin\help\php\desktop

#-------------------------------------------------------
# BIGHTML build
#-------------------------------------------------------

bightml: bightml_cleanbin bightml_images
  $(XSLTPROC) --xinclude --timing $(XSLSTYLE_BIGHTML) index.xml

bightml_cleanbin:
  -rd /s/q $(ROOT)\bin\help\desktop_big
  $(MAKE) dirs

bightml_images: dirs
  -rd /s/q $(ROOT)\bin\help\desktop_big\desktop_images
  xcopy /s/q $(ROOT)\src\desktop\help\desktop_images $(ROOT)\bin\help\desktop_big\desktop_images\ #
  copy $(ROOT)\src\desktop\help\topicresponse.js $(ROOT)\bin\help\desktop_big
  copy $(ROOT)\src\desktop\help\kmhelp.css $(ROOT)\bin\help\desktop_big

#-------------------------------------------------------
# HTML copy to website
#-------------------------------------------------------

install:
  if not exist $(KEYMAN_SITE_ROOT)\help.keyman.com\products\desktop exit 1
  -rd /s/q $(KEYMAN_SITE_ROOT)\help.keyman.com\products\desktop\$(KEYMAN_VERSION)\docs
  xcopy /s/q/y $(ROOT)\bin\help\php\desktop\* $(KEYMAN_SITE_ROOT)\help.keyman.com\products\desktop\$(KEYMAN_VERSION)\docs\ #

!include ..\..\Target.mak
