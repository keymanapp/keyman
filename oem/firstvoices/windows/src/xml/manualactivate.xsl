<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>
  
  <xsl:variable name="locale_manualactivate" select="$locale/Dialog[@Id='ManualActivate'][1]" />

  <xsl:template match="/">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title><xsl:value-of select="$locale/String[@Id='S_ManualActivate_Title']"/></title>
<style type="text/css">
    * { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; }

html { overflow: hidden }
body {
 font-size:		11px; 
 text-align:	justify;
 margin:	0px;
 width:         482px;
 overflow: hidden;
}
.button {
 font-size: 11px;
 height: 23px; width: 128px;
}
.button_cancel {
 font-size: 11px;
 height: 23px; width: 64px;
}

.licence {
 font: 12px Fixedsys;
}
li.step { font-weight: bold; padding: 10px 0px 10px 0px }
.detail { font-weight: normal; margin-top: 6px }
</style>
</head>

<body>
<div style="border: 1px solid #AD4A29; width: 480px;">
<div style='background: #AD4A29; text-align: right'>
  <img alt='Tavultesoft'>
    <xsl:attribute name='src'><xsl:value-of select='/Keyman/templatepath' />activate_logo.gif</xsl:attribute>
  </img>  
</div>
<div style="padding: 10px;">
<ol>
  <li class='step'><xsl:value-of select="$locale/String[@Id='S_ManualActivate_Save']"/><br />
    <div class='detail'>
      <input type="button" class='button' onclick="javascript:location.href='keyman:savetoclipboard'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_SaveToClipboard']"/></xsl:attribute>
      </input>
      <input type="button" class='button' onclick="javascript:location.href='keyman:savetodisk'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_SaveToDisk']"/></xsl:attribute>
      </input>
      <input type="button" class='button' onclick="javascript:location.href='keyman:savetofile'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_SaveToFile']"/></xsl:attribute>
      </input>
    </div>
  </li>

  <li class='step'><xsl:value-of select="$locale/String[@Id='S_ManualActivate_Send']"/><br />

    <div class='detail'><a href='keyman:help_send'><xsl:value-of select="$locale/String[@Id='S_ManualActivate_SendHelp']"/></a></div>
  </li>

  <li class='step'><xsl:value-of select="$locale/String[@Id='S_ManualActivate_Load']"/><br />
    <div class='detail'>
      <input type="button" class='button' onclick="javascript:location.href='keyman:loadfromclipboard'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_LoadFromClipboard']"/></xsl:attribute>
      </input>
      <input type="button" class='button' onclick="javascript:location.href='keyman:loadfromdisk'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_LoadFromDisk']"/></xsl:attribute>
      </input>
      <input type="button" class='button' onclick="javascript:location.href='keyman:loadfromfile'">
        <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_ManualActivate_LoadFromFile']"/></xsl:attribute>
      </input>
    </div>
  </li>
</ol>

<div style='text-align: right'><input type="button" class='button_cancel' onclick="javascript:location.href='keyman:cancel'">
  <xsl:attribute name="value"><xsl:value-of select="$locale/String[@Id='S_Button_Close']"/></xsl:attribute>
</input></div>

</div>
</div>
</body>

</html>
    </xsl:template>
  </xsl:stylesheet>