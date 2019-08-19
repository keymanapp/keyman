<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>
  <xsl:variable name="locale_splash" select="$locale/Dialog[@Id='Splash'][1]" />
  <xsl:template match="/">

<html>
<head>
  <meta http-equiv="X-UA-Compatible" content="IE=Edge"/>
  <title><xsl:value-of select="($locale/String[@Id='S_Splash_Title'])[1]" /></title>
  <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>menu.css</xsl:attribute></link>
  <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>splash.css</xsl:attribute></link>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>menu.js</xsl:attribute><xsl:text> </xsl:text></script>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>splash.js</xsl:attribute><xsl:text> </xsl:text></script>
</head>

<body>
<div id="size">
  <div id="ribbon">
    <div id="ribbon1"></div>
    <div id="ribbon2"></div>
    <div id="ribbon3"></div>
  </div>
  <div id="titleBox"></div>
  <div id="silLogo"></div>
  <div id="family"></div>
  
  <a href="keyman:start" id="startNow" class="button">
    <div class="btn btn-blue"><xsl:value-of select="$locale/String[@Id='S_Splash_Start']"/></div>
  </a>
  <a class="button" id="config" href="keyman:config"><div class="btn btn-orange"><xsl:value-of select="$locale/String[@Id='S_Splash_Configuration']"/></div></a>
  
  <a href="keyman:exit" id="exit" class="button"><div class="btn btn-small"><xsl:value-of select="$locale/String[@Id='S_Splash_Exit']"/></div></a>

  <div id="tasks">
    <div>
      <input type="checkbox" id="showAtStartupBox" checked='checked' onclick='toggleDisplay(this)' />
      <label for="showAtStartupBox"><xsl:value-of select="$locale/String[@Id='S_Splash_ShowAtStartup']"/></label>
    </div>
  </div>
  
  <div id="keyboards">
    <xsl:apply-templates select="//KeymanKeyboardInstalled[loaded]"/>
    <xsl:if test="count(//KeymanKeyboardInstalled[loaded]) = 0">No keyboards loaded - <a href="keyman:config">Open Configuration</a></xsl:if>
  </div>
 
  <div id="version"><xsl:value-of select="/Keyman/Version"/>
    <span id="beta"><xsl:value-of select="$locale/String[@Id='S_BETA']"/></span>
  </div>
</div>
</body>
</html>
  </xsl:template>
  
<xsl:template match="KeymanKeyboardInstalled">
  <div><xsl:value-of select="name" /></div>
</xsl:template>
    
</xsl:stylesheet>
