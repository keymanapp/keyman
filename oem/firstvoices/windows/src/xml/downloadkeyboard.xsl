<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="locale_downloadkeyboard" select="$locale/Dialog[@Id='DownloadKeyboard'][1]" />
  <xsl:variable name="locale_downloadlocale" select="$locale/Dialog[@Id='DownloadLocale'][1]" />
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:k="http://www.tavultesoft.com/xml/70">
      <head>
        <xsl:choose>
          <xsl:when test="/Keyman/DownloadLocale">
            <title><xsl:value-of select="$locale/String[@Id='S_DownloadLocale_Title']"/></title>
          </xsl:when>
          <xsl:otherwise>
            <title><xsl:value-of select="$locale/String[@Id='S_DownloadKeyboard_Title']"/></title>
           </xsl:otherwise>
        </xsl:choose>
        
        <style type="text/css">
            * { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; }
            
            body { padding: 0px; margin: 0px; overflow: hidden; 
              <xsl:choose>
                <xsl:when test="/Keyman/DownloadLocale">
                  width: <xsl:value-of select="$locale_downloadlocale/@Width" />px; 
                  height: <xsl:value-of select="$locale_downloadlocale/@Height" />px;
                </xsl:when>
                <xsl:otherwise>
                  width: <xsl:value-of select="$locale_downloadkeyboard/@Width" />px; 
                  height: <xsl:value-of select="$locale_downloadkeyboard/@Height" />px;
                </xsl:otherwise>
              </xsl:choose>
               }
            html { width: 100%; padding: 0px; margin: 0px; overflow: hidden }
            
            #size {
              position: absolute;
              <xsl:choose>
                <xsl:when test="/Keyman/DownloadLocale">
                  width: <xsl:value-of select="$locale_downloadlocale/@Width" />px; 
                  height: <xsl:value-of select="$locale_downloadlocale/@Height" />px;
                </xsl:when>
                <xsl:otherwise>
                  width: <xsl:value-of select="$locale_downloadkeyboard/@Width" />px; 
                  height: <xsl:value-of select="$locale_downloadkeyboard/@Height" />px;
                </xsl:otherwise>
              </xsl:choose>
            }

            #contentframe {
              position: absolute;
              left: 0;
              top: 0;
              width: 100%;
              height: expression(document.body.clientHeight-<xsl:value-of select="$locale_downloadkeyboard/footerheight"/>);
              overflow: hidden;
              x-background: #FFFFFF;
              border-bottom: 2px solid #888888; 
              }
            #footerframe {
              position: absolute;
              left: 0px;
              top: expression(document.body.clientHeight-<xsl:value-of select="$locale_downloadkeyboard/footerheight"/>);
              height: <xsl:value-of select="$locale_downloadkeyboard/footerheight"/>px;
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
        <xsl:choose>
          <xsl:when test="/Keyman/DownloadLocale">
            <iframe id="contentframe" frameborder="0">
              <xsl:attribute name="src">http://www.tavultesoft.com/prog/70/downloadlocales/?version=<xsl:value-of select="/Keyman/Version" /></xsl:attribute>&#160;
            </iframe>
          </xsl:when>
          <xsl:otherwise>
            <iframe id="contentframe" frameborder="0">
              <xsl:attribute name="src">http://www.tavultesoft.com/prog/80/downloadkeyboards/?version=<xsl:value-of select="/Keyman/Version" /></xsl:attribute>&#160;
						</iframe>
          </xsl:otherwise>
        </xsl:choose>
				<p>ha!</p>
        <div id="footerframe">
          <div style="float:left; font-size: 13.3px;">
            <input type="checkbox" id="chkDownloadOnly" /> <label for="chkDownloadOnly"><xsl:value-of select="$locale/String[@Id='S_DownloadKeyboard_DownloadOnlyCheckbox']"/></label>
          </div>
          <div style="float:right">
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
