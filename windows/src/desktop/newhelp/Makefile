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

#-------------------------------------------------------
# Main targets
#-------------------------------------------------------

build: chm html
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

#-------------------------------------------------------
# CHM build
#-------------------------------------------------------

chm:  keymandesktop.chm

keymandesktop.hhp:  index.htm

.xml.htm:
  c:\bin\libxslt\xsltproc --xinclude --timing --output $@ $(XSLSTYLE_CHM) $<

.hhp.chm:
	-1 "c:\program files\html help workshop\hhc" $<

#-------------------------------------------------------
# HTML build
#-------------------------------------------------------

html: html_cleanbin html_images
  c:\bin\libxslt\xsltproc --xinclude --timing $(XSLSTYLE_HTML) index.xml

html_cleanbin:
  -rd /s/q $(ROOT)\bin\help\desktop
  $(MAKE) dirs

html_images: dirs
  -rd /s/q $(ROOT)\bin\help\desktop\images
  xcopy /s/q $(ROOT)\src\desktop\help\images $(ROOT)\bin\help\desktop\images\ #
  copy $(ROOT)\src\desktop\help\topicresponse.js $(ROOT)\bin\help\desktop
  copy $(ROOT)\src\desktop\help\kmhelp.css $(ROOT)\bin\help\desktop

#-------------------------------------------------------
# BIGHTML build
#-------------------------------------------------------

bightml: bightml_cleanbin bightml_images
  c:\bin\libxslt\xsltproc --xinclude --timing $(XSLSTYLE_BIGHTML) index.xml

bightml_cleanbin:
  -rd /s/q $(ROOT)\bin\help\desktop_big
  $(MAKE) dirs

bightml_images: dirs
  -rd /s/q $(ROOT)\bin\help\desktop_big\images
  xcopy /s/q $(ROOT)\src\desktop\help\images $(ROOT)\bin\help\desktop_big\images\ #
  copy $(ROOT)\src\desktop\help\topicresponse.js $(ROOT)\bin\help\desktop_big
  copy $(ROOT)\src\desktop\help\kmhelp.css $(ROOT)\bin\help\desktop_big

#-------------------------------------------------------
# HTML copy to website
#-------------------------------------------------------

install:
  -rd /s/q s:\tavultesoft\website\dec2004\root\keyman\documentation\70
  xcopy /s/q $(ROOT)\bin\help\desktop\* s:\tavultesoft\website\dec2004\root\keyman\documentation\70\ #

!include ..\..\Target.mak
