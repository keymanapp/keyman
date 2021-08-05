<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="dialoginfo_onlineupdate" select="$dialoginfo/Dialog[@Id='OnlineUpdate'][1]" />

  <xsl:template match="/">

<html>
<head>
  <meta http-equiv="X-UA-Compatible" content="IE=Edge"/>
  <script src="/app/sentry.bundle.min.js"></script>
  <script src="/app/sentry.init.js"></script>
<title><xsl:value-of select="$locale/string[@name='S_Update_Title']"/></title>
<style type="text/css">

* { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI"; }

html { overflow: hidden }

body {
 font-size:		11px;
 text-align:	justify;
 margin:	0px;
 width:         469px;
 overflow: hidden;
}

.button {
 font: 11px <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI";
 height: 23px; width: 128px;
}

.button img { display: none; }

<xsl:if test="/Keyman/osversion/@index >= 9">
  .shieldButton {  }
  .shieldButton img { display: inline !important; }
</xsl:if>

div {
  font: 13.3px <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI";
}

#border {
  border: none;
  width: 469px;
  height: 375px;
  }

#header { background: white;  }

#content {
 padding: 10px;
 text-align: center;
}

#footer {
  position: absolute;
  bottom: 10px;
  text-align: center;
  width: 100%;
}

#UpdateContainer {
	height: 220px;
	overflow-y: auto;
	border: solid 1px gray;
	margin: 12px 8px;
}

#Updates { border-collapse: collapse; width: 100% }
#Updates td, #Updates th { font-size: 11px; border: solid 1px #c9c9c9; padding: 1px 4px }
#Updates th { font-weight: bold; text-align: left; white-space: nowrap }
#Updates td { text-align: left; }

#Updates td label { display: block; }

#Updates td.UpdateCheckBox { width: 22px }

#NewVersionAvailable { font-weight: bold; font-size: 13.3px; text-align: left; margin-bottom: 4px }
#NewVersionText { margin-bottom: 12px; text-align: left; font-size: 13.3px }

</style>
				<script type="text/javascript">
					document.onkeydown = function()
					{
						if(event.keyCode == 13 <![CDATA[&&]]> (!event.srcElement.type || event.srcElement.type != 'button') <![CDATA[&&]]> !document.getElementById('submitButton').disabled)
						{
							event.cancelBubble = true; event.returnValue = false;
							location.href='keyman:installnow';
						}
						else if(event.keyCode == 27 )
						{
							event.cancelBubble = true; event.returnValue = false;
							location.href='keyman:installlater';
						}
					}
          function updateTick(id)
          {
            enableControls();
            var e = document.getElementById('Update_'+id);
            if(!e) return;
            if(e.checked) location.href='keyman:tickupdate?id='+id;
            else location.href='keyman:untickupdate?id='+id;
          }
          function enableControls()
          {
            var e = false, admin = false;
            var elems = document.getElementsByTagName('input');
            for(var i = 0; i <![CDATA[<]]> elems.length; i++)
            {
              if(elems[i].type != 'checkbox') continue;
              if(elems[i].checked)
              {
                e = true;
                if(document.getElementById(elems[i].id+'_RequiresAdmin')) admin = true;
              }
            }
            with(document.getElementById('submitButton'))
            {
              disabled = !e;
              className = admin ? 'button shieldButton' : 'button';
            }
          }
				</script>
</head>

<body onload="javascript:enableControls()">
<div id="border">
  <div id="header">
    <img alt='Keyman' src="/app/keyman-desktop.png" />
  </div>
  <div id='content'>
    <div id="NewVersionAvailable"><xsl:copy-of select="($locale/string[@name='S_Update_NewVersionAvailable'])[1]" /></div>
    <div id="NewVersionText"><xsl:copy-of select="($locale/string[@name='S_Update_NewVersionPrompt'])[1]"/></div>
    <div id="UpdateContainer">
			<table cellspacing="0" cellpadding="0" id="Updates">
        <tr>
          <th></th>
          <th><xsl:value-of select="$locale/string[@name='S_Update_ComponentHead']"/></th>
          <th><xsl:value-of select="$locale/string[@name='S_Update_OldVersionHead']"/></th>
          <th><xsl:value-of select="$locale/string[@name='S_Update_SizeHead']"/></th>
        </tr>
				<xsl:apply-templates select="//Update" />
			</table>
    </div>
  </div>
  <div id="footer">
    <button type="submit" id="submitButton" class='button shieldButton' onclick="javascript:location.href='keyman:installnow'">
      <img alt="" style="vertical-align:middle; width: 16px; margin: 0 4px 0 2px;" src="/app/shield.png" />
      <xsl:value-of select="$locale/string[@name='S_Update_Button_InstallNow']"/>
    </button>
    &#160;
    <input type="button" class='button' onclick="javascript:location.href='keyman:installlater'">
      <xsl:attribute name="value"><xsl:value-of select="$locale/string[@name='S_Update_Button_InstallLater']"/></xsl:attribute>
    </input>
  </div>
</div>
</body>
</html>
    </xsl:template>

  <xsl:template match="//Update">
    <tr>
      <td class="UpdateCheckBox">
				<input type="checkbox">
					<xsl:attribute name="onclick">javascript:updateTick("<xsl:value-of select="index" />");</xsl:attribute>
					<xsl:attribute name="id">Update_<xsl:value-of select="index" /></xsl:attribute>
					<xsl:if test="not(@DefaultUnchecked)"><xsl:attribute name="checked">checked</xsl:attribute></xsl:if>
					<xsl:attribute name="name">Update_<xsl:value-of select="index" /></xsl:attribute>
				</input>
        <xsl:if test="RequiresAdmin">
          <span><xsl:attribute name="id">Update_<xsl:value-of select="index" />_RequiresAdmin</xsl:attribute></span>
        </xsl:if>
      </td>
    <xsl:choose>
      <xsl:when test="Package/Text != ''">
        <td class='component'>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Package/Text"/>
          </label>
        </td>
        <td>
          <xsl:value-of select="Package/OldVersion"/>
        </td>
        <td>
          <xsl:value-of select="Package/DownloadSize"/>
        </td>
      </xsl:when>
      <xsl:when test="Keyman/DownloadURL != ''">
        <td>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Keyman/Text"/>
          </label>
        </td>
        <td>
          <xsl:value-of select="Keyman/OldVersion"/>
        </td>
        <td>
          <xsl:value-of select="Keyman/DownloadSize"/>
        </td>
      </xsl:when>
      <xsl:otherwise>
        <td>
          <div id="DownloadFrom">
            <xsl:value-of select="$locale/string[@name='S_Update_DownloadFrom']"/>
          </div>
          <div id="URL">
            <a href="keyman:openwebsite">
              <xsl:value-of select="/Keyman/DownloadURL"/>
            </a>
          </div>
        </td>
        <td>
          <xsl:value-of select="Keyman/DownloadVersion"/>
        </td>
      </xsl:otherwise>
    </xsl:choose>
    </tr>
  </xsl:template>
</xsl:stylesheet>
