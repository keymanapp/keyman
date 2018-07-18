﻿<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  <xsl:import href="elements.xsl"/>

  <xsl:import href="welcome.xsl"/>
  <xsl:import href="keyboards.xsl"/>
  <xsl:import href="packages.xsl"/>
  <xsl:import href="distribution.xsl"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:k="http://www.tavultesoft.com/xml/70">
      <xsl:call-template name="head" />
      <body onload="pageload()">
        <div id="state"><xsl:copy-of select="/KeymanDeveloperProject/ViewState" /></div>
        <div id="menubackground" onmousedown="HideMenu()"></div>
        <div id="pageheader">
          <div id="upperexpand" class="checkbox checked" onmousedown="javascript:showhideupper();">Show help</div>
          <div id="currentpage">Project - Welcome </div>
        </div>
        <div id="pages" class="pages">
          <xsl:call-template name="page_welcome" />
          <xsl:call-template name="page_keyboard" />
          <xsl:call-template name="page_packaging" />
          <xsl:call-template name="page_distribution" />
        </div>
        <div id="tabs" class="tabbackground">
          <div class="tabbspacer"></div>

          <div id="tabb0" class="tabb" onmousedown="javascript:selecttabb(0);">
            <img alt="Welcome" src='res/tab_welcome.gif' />
            Welcome
          </div>
          
          <div id="tabb1" class="tabb" onmousedown="javascript:selecttabb(1);">
            <img alt="Keyboards" src='res/tab_keyboard.png' />
            Keyboards
          </div>

          <div id="tabb2" class="tabb" onmousedown="javascript:selecttabb(2);">
            <img alt="Packaging" src='res/tab_package.png' />
            Packaging
          </div>

          <div id="tabb3" class="tabb" onmousedown="javascript:selecttabb(3);">
            <img alt="Distribution" src='res/tab_distrib.png' />
            Distribution
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>