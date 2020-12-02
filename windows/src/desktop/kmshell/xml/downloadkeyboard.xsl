<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="dialoginfo_downloadkeyboard" select="$dialoginfo/Dialog[@Id='DownloadKeyboard'][1]" />

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
        <script src="/app/jquery.min.js"></script>
        <title><xsl:value-of select="$locale/string[@name='S_DownloadKeyboard_Title']"/></title>
        <link rel="stylesheet" type="text/css" href="/app/config.css" />
        <style type="text/css">
            * { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI"; }

            body {
              padding: 0px; margin: 0px; overflow: hidden;
              width: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Width" />px;
              height: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Height" />px;
            }
            html { width: 100%; padding: 0px; margin: 0px; overflow: hidden }

            #size {
              position: absolute;
              width: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Width" />px;
              height: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Height" />px;
            }

            #contentframe {
              position: absolute;
              left: 0;
              top: 0;
              width: 100%;
              height: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Height - $dialoginfo_downloadkeyboard/footerheight" />px;
              border-bottom: 2px solid #888888;
              overflow: scroll;
              }
            #footerframe {
              position: absolute;
              left: 0px;
              top: <xsl:value-of select="$dialoginfo_downloadkeyboard/@Height - $dialoginfo_downloadkeyboard/footerheight" />px;
              height: <xsl:value-of select="$dialoginfo_downloadkeyboard/footerheight" />px;
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

  // This function is called by the Delphi code when browse events
  // happen, because we cannot determine the state of history adequately
  // from within the browser context.
  function updateBackButtonState(shouldEnable) {
    document.getElementById('button_back').disabled = !shouldEnable;
  }

  function goBack() {
    history.back();
  }
]]></script>

      </head>
      <body>
        <iframe id="contentframe" frameborder="0">
          <xsl:attribute name="src"><xsl:value-of select='/Keyman/keyman-com' />/go/windows/<xsl:value-of select="/Keyman/version-info/@versionRelease" />/download-keyboards?version=<xsl:value-of select="/Keyman/Version" /></xsl:attribute>&#160;
        </iframe>
        <div id="footerframe">
          <div style="float:left; padding-left: 8px">
            <xsl:call-template name="button">
              <xsl:with-param name="id">back</xsl:with-param>
              <xsl:with-param name="disabled">1</xsl:with-param>
              <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Back']"/></xsl:with-param>
              <xsl:with-param name="command">javascript:goBack()</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
          </div>
          <div style="float:right; padding-right: 8px">
            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Cancel']"/></xsl:with-param>
              <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
