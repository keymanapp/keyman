<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="locale_downloadkeyboard" select="$locale/Dialog[@Id='DownloadKeyboard'][1]" />
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />
        <title><xsl:value-of select="$locale/String[@Id='S_DownloadKeyboard_Title']"/></title>
        
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>config.css</xsl:attribute></link>
        <style type="text/css">
            * { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; }
            
            body { 
              padding: 0px; margin: 0px; overflow: hidden; 
              width: <xsl:value-of select="$locale_downloadkeyboard/@Width" />px; 
              height: <xsl:value-of select="$locale_downloadkeyboard/@Height" />px;
            }
            html { width: 100%; padding: 0px; margin: 0px; overflow: hidden }
            
            #size {
              position: absolute;
              width: <xsl:value-of select="$locale_downloadkeyboard/@Width" />px; 
              height: <xsl:value-of select="$locale_downloadkeyboard/@Height" />px;
            }

            #contentframe {
              position: absolute;
              left: 0;
              top: 0;
              width: 100%;
              height: <xsl:value-of select="$locale_downloadkeyboard/@Height - $locale_downloadkeyboard/footerheight" />px;
              border-bottom: 2px solid #888888;
              overflow: scroll;
              }
            #footerframe {
              position: absolute;
              left: 0px;
              top: <xsl:value-of select="$locale_downloadkeyboard/@Height - $locale_downloadkeyboard/footerheight" />px;
              height: <xsl:value-of select="$locale_downloadkeyboard/footerheight" />px;
              width: 100%;
              overflow: hidden;
              background: #dcdcdc;
              padding: 8px 8px 8px 8px;
            }
        </style>
<script type="text/javascript"><![CDATA[
	document.onkeydown = function()
	{
	  if(event.keyCode == 27) location.href='keyman:footer_cancel';
		else return;
		event.cancelBubble = true; event.returnValue = false;
  }
]]></script>
				
      </head>
      <body>
        <div id="size"></div>
        <iframe id="contentframe" frameborder="0">
          <xsl:attribute name="src">https://keyman.com/go/desktop/10.0/download-keyboards?version=<xsl:value-of select="/Keyman/Version" /></xsl:attribute>&#160;
        </iframe>
        <div id="footerframe">
          <!--<div style="float:left; font-size: 13.3px;">
            <input type="checkbox" id="chkDownloadOnly" /> <label for="chkDownloadOnly"><xsl:value-of select="$locale/String[@Id='S_DownloadKeyboard_DownloadOnlyCheckbox']"/></label>
          </div>-->
          <div style="float:right; padding-right: 8px">
            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
              <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
