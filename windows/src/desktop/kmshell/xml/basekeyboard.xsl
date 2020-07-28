<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="locale_basekeyboard" select="$locale/Dialog[@Id='BaseKeyboard'][1]" />

  <xsl:template match="/">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
    <script src="/app/sentry.bundle.min.js"></script>
    <script src="/app/sentry.init.js"></script>
    <title><xsl:value-of select="$locale/String[@Id='S_BaseKeyboard_Title']"/></title>
    <link rel="stylesheet" type="text/css" href="/app/config.css" />
<style type="text/css">
  * { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />, "Segoe UI";}

html {
 font-size:		 11px;
 text-align:	justify;
 margin: 	 0px;
 padding: 0;
 width:    <xsl:value-of select="$locale_basekeyboard/@Width" />px;
 height:   <xsl:value-of select="$locale_basekeyboard/@Height" />px;
 overflow: hidden;
}

body {
 font-size:	 	 11px;
 text-align:	justify;
 padding: 0;
 margin:	 0px;
 width:    <xsl:value-of select="$locale_basekeyboard/@Width" />px;
 height:   <xsl:value-of select="$locale_basekeyboard/@Height" />px;
 overflow: hidden;
}

p {
  text-align: left;
}

.button {
 font-size: 11px;
 height: 23px; width: 72px;
}

div {
  font-size: 13.3px;
}

#border {
  border: 1px solid #AD4A29;
  position: absolute;
  left: 0;
  top: 0;
  width: <xsl:value-of select="$locale_basekeyboard/@Width - 2" />px;
  height: <xsl:value-of select="$locale_basekeyboard/@Height - 2" />px;
  }

#content {
 padding: 10px;
 text-align: center;
}

#footer {
  position: absolute;
  top: expression(<xsl:value-of select="$locale_basekeyboard/@Height" /> - 36);
  height: 36px;
  text-align: center;
  width: 100%;
}

.form { position: absolute; display: block; vertical-align: middle; height: 24px; }
#ProxyServerLabel { left: 16px; top: 20px;}
#ProxyPortLabel { left: 16px; top: 48px; }
#ProxyServer { left: 88px; top: 16px;}
#ProxyPort { left: 88px; top: 44px; }
#ProxyUsernameLabel { left: 16px; top: 76px; }
#ProxyUsername { left: 88px; top: 72px; }
#ProxyPasswordLabel { left: 16px; top: 104px; }
#ProxyPassword      { left: 88px; top: 100px; }

#Form_ProxyServer, #Form_ProxyPort, #Form_ProxyUsername, #Form_ProxyPassword {
  width: 136px;
}

</style>
<script type="text/javascript">
	<xsl:text disable-output-escaping="yes"><![CDATA[
  function ok()
  {
    location.href='keyman:footer_ok?'+
      'id='+document.getElementById('options_base_keyboard_select').value;
  }

	document.onkeydown = function()
	{
		if(event.keyCode == 13 && (!event.srcElement.type || event.srcElement.type != 'button')) ok();
	  else if(event.keyCode == 27) location.href='keyman:footer_cancel';
		else return;
		event.cancelBubble = true; event.returnValue = false;
  }
]]></xsl:text></script>
</head>
<body>
<div id="border">
  <div id='content'>

    <p><xsl:value-of select="$locale/String[@Id='S_BaseKeyboard_Text']"/></p>

    <div id="options_base_keyboard">
      <select id='options_base_keyboard_select'>
        <xsl:for-each select="//BaseKeyboards/BaseKeyboard">
          <option>
            <xsl:if test="selected"><xsl:attribute name="selected">selected</xsl:attribute></xsl:if>
            <xsl:attribute name="value"><xsl:value-of select="id"/></xsl:attribute>
            <xsl:value-of select="name" />
          </option>
        </xsl:for-each>
      </select>
    </div>
  </div>
  <div id="footer">
    <xsl:call-template name="button">
      <xsl:with-param name="default">1</xsl:with-param>
      <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_OK']"/></xsl:with-param>
      <xsl:with-param name="command">javascript:ok()</xsl:with-param>
      <xsl:with-param name="width">100px</xsl:with-param>
    </xsl:call-template>

    <xsl:call-template name="button">
      <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
      <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
      <xsl:with-param name="width">100px</xsl:with-param>
    </xsl:call-template>
  </div>
</div>
</body>
</html>
    </xsl:template>
  </xsl:stylesheet>
