<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="dialoginfo_proxyconfiguration" select="$dialoginfo/Dialog[@Id='ProxyConfiguration'][1]" />

  <xsl:template match="/">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <script src="/app/sentry.bundle.min.js"></script>
  <script src="/app/sentry.init.js"></script>
<title><xsl:value-of select="$locale/string[@name='S_ProxyConfiguration_Title']"/></title>
<style type="text/css">
  * { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI"; }

html {
 font-size:		 13px;
 text-align:	justify;
 margin: 	 0px;
 width:    <xsl:value-of select="$dialoginfo_proxyconfiguration/@Width" />px;
 height:   <xsl:value-of select="$dialoginfo_proxyconfiguration/@Height" />px;
 overflow: hidden;
}

body {
 font-size:	 	 13px;
 text-align:	justify;
 margin:	 0px;
 width:    <xsl:value-of select="$dialoginfo_proxyconfiguration/@Width" />px;
 height:   <xsl:value-of select="$dialoginfo_proxyconfiguration/@Height" />px;
 overflow: hidden;
}

.button {
 font-size: 11px;
 height: 23px; width: 72px;
}

div {
  font-size: 13px;
}

#border {
  border: 1px solid #AD4A29;
  width: <xsl:value-of select="$dialoginfo_proxyconfiguration/@Width - 2" />px;
  height: <xsl:value-of select="$dialoginfo_proxyconfiguration/@Height - 2" />px;
  }
#content {
 padding: 10px;
 text-align: center;
}

#footer {
  position: absolute;
  bottom: 0px;
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
      'server='+document.getElementById('Form_ProxyServer').value+
      '&port='+document.getElementById('Form_ProxyPort').value+
      '&username='+document.getElementById('Form_ProxyUsername').value+
      '&password='+document.getElementById('Form_ProxyPassword').value;
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
    <div class='form' id="ProxyServerLabel"><xsl:value-of select="$locale/string[@name='S_ProxyConfiguration_Server']" /></div>
    <div class='form' id="ProxyServer">
      <input type="text" id="Form_ProxyServer">
        <xsl:attribute name="value"><xsl:value-of select="/Keyman/Proxy/Server"/></xsl:attribute>
      </input>
    </div>

    <div class='form' id="ProxyPortLabel"><xsl:value-of select="$locale/string[@name='S_ProxyConfiguration_Port']" /></div>
    <div class='form' id="ProxyPort">
      <input type="text" id="Form_ProxyPort">
        <xsl:attribute name="value"><xsl:value-of select="/Keyman/Proxy/Port"/></xsl:attribute>
      </input>
    </div>

    <div class='form' id="ProxyUsernameLabel"><xsl:value-of select="$locale/string[@name='S_ProxyConfiguration_Username']" /></div>
    <div class='form' id="ProxyUsername">
      <input type="text" id="Form_ProxyUsername">
        <xsl:attribute name="value"><xsl:value-of select="/Keyman/Proxy/Username"/></xsl:attribute>
      </input>
    </div>

    <div class='form' id="ProxyPasswordLabel"><xsl:value-of select="$locale/string[@name='S_ProxyConfiguration_Password']" /></div>
    <div class='form' id="ProxyPassword">
      <input type="password" id="Form_ProxyPassword">
        <xsl:attribute name="value"><xsl:value-of select="/Keyman/Proxy/Password"/></xsl:attribute>
      </input>
    </div>
  </div>
  <div id="footer">
    <input type="submit" class='button' onclick="javascript:ok()">
      <xsl:attribute name="value"><xsl:value-of select="$locale/string[@name='S_Button_OK']"/></xsl:attribute>
    </input>
    <input type="button" class='button' onclick="javascript:location.href='keyman:footer_cancel'">
      <xsl:attribute name="value"><xsl:value-of select="$locale/string[@name='S_Button_Cancel']"/></xsl:attribute>
    </input>
  </div>
</div>
</body>
</html>
    </xsl:template>
  </xsl:stylesheet>
