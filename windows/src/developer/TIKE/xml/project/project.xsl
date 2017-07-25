<?xml version="1.0" encoding="utf-8" ?>

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
        <div id="state"><xsl:value-of select="/KeymanDeveloperProject/state" /></div>
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
            <img alt="Welcome">
              <xsl:attribute name="src"><xsl:value-of select='/KeymanDeveloperProject/templatepath'/>tab_welcome.gif</xsl:attribute>
            </img>
            Welcome
          </div>
          
          <div id="tabb1" class="tabb" onmousedown="javascript:selecttabb(1);">
            <img alt="Keyboards">
              <xsl:attribute name="src"><xsl:value-of select='/KeymanDeveloperProject/templatepath'/>tab_keyboard.png</xsl:attribute>
            </img>
            Keyboards
          </div>

          <div id="tabb2" class="tabb" onmousedown="javascript:selecttabb(2);">
            <img alt="Packaging">
              <xsl:attribute name="src"><xsl:value-of select='/KeymanDeveloperProject/templatepath'/>tab_package.png</xsl:attribute>
            </img>
            Packaging
          </div>

          <div id="tabb3" class="tabb" onmousedown="javascript:selecttabb(3);">
            <img alt="Distribution">
              <xsl:attribute name="src"><xsl:value-of select='/KeymanDeveloperProject/templatepath'/>tab_distrib.png</xsl:attribute>
            </img>
            Distribution
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>