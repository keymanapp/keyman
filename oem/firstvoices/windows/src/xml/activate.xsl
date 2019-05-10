<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="locale_activate" select="$locale/Dialog[@Id='Activate'][1]" />
  
  <xsl:template match="/">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title><xsl:value-of select="$locale/String[@Id='S_Activate_Title']"/></title>
<style type="text/css">

* { font-family: Tahoma; }
html, body {
  font: 12px Tahoma;
  margin: 0px;
  padding: 0;
  overflow: hidden;
}
div { position: static; }

#size {
  position: absolute; left: 0; top: 0; border: solid 1px #a53f20;
  background: white url(<xsl:value-of select="/Keyman/templatepath"/>swoosh.png) bottom left repeat-x;
  width: 700px;
  height: 420px;
}
#caption {
  position: absolute;
  left: 0;
  top: 0;
  background: url(<xsl:value-of select="/Keyman/templatepath"/>banner-bg.png) #ad4a28;
  height: 63px;
  width: 698px;
}
#displaylanguage { top: 40px; right: 12px; position: absolute; color: white; }
#displaylanguage a { color: white; }

#titleBox { position: absolute; left: 38px; top: 100px; background: url(<xsl:value-of select="/Keyman/templatepath"/>activate-title.png); width: 388px; height: 37px; }

#activateBox {
  position: absolute;
  top: 154px; 
  left: 67px;
  width: 80%;
}

.licence {
  padding: 10px 0 10px 40px;
  font: 12px Fixedsys;
}
.licence input { font: 12px Fixedsys; margin-left: -5px; padding: 2px 5px; border: 1px solid #3e82dd; width: 204px; height: 21px; }

#clickLine { position: absolute; top: 280px; left: 67px; }
#manualLine { position: absolute; top: 307px; left: 67px; }

#linksBox { position: absolute; top: 349px; left: 67px; }
#linksShield { vertical-align: middle; height: 16px; }

#btnActivate { top: 340px; left: 371px; }
#btnCancel { top: 340px; left: 463px; }

a.button {
  cursor: pointer;
  text-align: center;
  color: black;
  text-decoration: none;
  position: absolute;
}
a.button:hover { color: #ad4a28; }

.btn-blue-small-left { float: left; width: 3px; height: 34px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-blue-small-left.png); }
.btn-blue-small { float: left; width: 77px; height: 26px; padding-top: 8px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-blue-small-mid.png); text-align: center; }
.btn-blue-small-right { float: left; width: 3px; height: 34px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-blue-small-right.png); }
.btn-grey-left { float: left; width: 3px; height: 34px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-grey-left.png); }
.btn-grey { float: left; width: 77px; height: 26px; padding-top: 8px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-grey-mid.png) top left repeat-x; text-align: center; }
.btn-grey-right { float: left; width: 3px; height: 34px; background: url(<xsl:value-of select="/Keyman/templatepath"/>btn-grey-right.png); }

</style>
</head>

<body>
<div id="size">
<div id="caption">
</div>

  <div id="titleBox" title="Activate Keyman Desktop"></div>
<div id="activateBox">
<xsl:value-of select="$locale/String[@Id='S_Activate_Description']"/>
<div class="licence">ABC1-D2E3-FGHJ-KLMN-OPQR</div>
<xsl:value-of select="$locale/String[@Id='S_Activate_Enter']"/>
  <div class="licence">
    <form action="keyman:activate" name="licenceform" style="margin: 0;">
      <input name="code" size="24">
        <xsl:attribute name="value"><xsl:value-of select="/Keyman/licencenumber"/></xsl:attribute>
      </input>
    </form>
  </div>
</div>
<div id="clickLine"><xsl:value-of select="$locale/String[@Id='S_Activate_ActivateOnline']"/></div>
<div id="manualLine"><xsl:value-of select="$locale/String[@Id='S_Activate_ActivateOffline']"/></div>

<div id="linksBox">
  <a href="javascript:navigate('keyman:manualactivation?code='+document.licenceform.code.value);"><xsl:value-of select="$locale/String[@Id='S_Activate_ManualActivation']"/></a>
  <xsl:if test="/Keyman/canelevate">
    <a href="javascript:navigate('keyman:activate?code='+document.licenceform.code.value);"><xsl:value-of select="$locale/String[@Id='S_Activate_ThisUser']"/></a>
  </xsl:if>
</div>

<a href="#" class="button" id="btnActivate">
  <xsl:choose>
    <xsl:when test="/Keyman/canelevate">
      <xsl:attribute name="onclick">javascript:navigate('keyman:elevatedactivation?code='+document.licenceform.code.value);</xsl:attribute>
      <div class="btn-blue-small-left"></div><div class="btn-blue-small">
        <img id="linksShield" alt=""><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>shield.png</xsl:attribute></img>
        <xsl:value-of select="$locale/String[@Id='S_Activate_Button_Activate']"/>
      </div><div class="btn-blue-small-right"></div>        
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="onclick">javascript:navigate('keyman:activate?code='+document.licenceform.code.value);</xsl:attribute>
      <div class="btn-blue-small-left"></div><div class="btn-blue-small"><xsl:value-of select="$locale/String[@Id='S_Activate_Button_Activate']"/></div><div class="btn-blue-small-right"></div>
    </xsl:otherwise>
  </xsl:choose>
</a>
<a href="#" class="button" id="btnCancel" onclick="javascript:navigate('keyman:cancel')"><div class="btn-grey-left"></div><div class="btn-grey"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></div><div class="btn-grey-right"></div></a>
 
</div>
</body>
</html>
    
    </xsl:template>
  </xsl:stylesheet>
