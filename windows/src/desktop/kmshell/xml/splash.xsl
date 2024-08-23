<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>
  <xsl:variable name="dialoginfo_splash" select="$dialoginfo/Dialog[@Id='Splash'][1]" />
  <xsl:template match="/">

<html>
<head>
  <script src="/app/sentry.bundle.min.js"></script>
  <script src="/app/sentry.init.js"></script>
  <title><xsl:value-of select="($locale/string[@name='S_Splash_Name'])[1]" />&#160;<xsl:value-of select="/Keyman/version-info/@versionRelease" /></title>
  <style> * { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI";} </style>
  <link rel="stylesheet" type="text/css" href="/app/menu.css" />
  <link rel="stylesheet" type="text/css" href="/app/splash.css" />
  <script src="/app/menu.js"></script>
  <script src="/app/splash.js"></script>
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
    <div class="btn btn-blue">
      <xsl:variable name="originalText" select="$locale/string[@name='S_Splash_Start_2']"/>
      <xsl:variable name="replacement" select="'Keyman'"/>
      <xsl:variable name="beforePlaceholder" select="substring-before($originalText, '%0:s')"/>
      <xsl:variable name="afterPlaceholder" select="substring-after($originalText, '%0:s')"/>
      <xsl:value-of select="concat($beforePlaceholder, $replacement, $afterPlaceholder)"/>
    </div>
  </a>
  <a class="button" id="config" href="keyman:config"><div class="btn btn-orange"><xsl:value-of select="$locale/string[@name='S_Splash_Configuration']"/></div></a>

  <a href="keyman:exit" id="exit" class="button"><div class="btn btn-small"><xsl:value-of select="$locale/string[@name='S_Splash_Exit']"/></div></a>

  <div id="tasks">
    <div>
      <input type="checkbox" id="showAtStartupBox" checked='checked' onclick='toggleDisplay(this)' />
      <label for="showAtStartupBox"><xsl:value-of select="$locale/string[@name='S_Splash_ShowAtStartup']"/></label>
    </div>
  </div>

  <div id="keyboards">
    <xsl:apply-templates select="//KeymanKeyboardInstalled[loaded]"/>
    <xsl:if test="count(//KeymanKeyboardInstalled[loaded]) = 0">No keyboards loaded - <a href="keyman:config">Open Configuration</a></xsl:if>
  </div>

  <div id="version"><xsl:value-of select="/Keyman/Version"/></div>
</div>
</body>
</html>
  </xsl:template>

<xsl:template match="KeymanKeyboardInstalled">
  <div><xsl:value-of select="name" /></div>
</xsl:template>

</xsl:stylesheet>
