<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:include href="elements.xsl"/>
  
  <xsl:variable name="locale_splash" select="$locale/Dialog[@Id='Splash'][1]" />

  <xsl:template match="/">

<html>
<head>
  <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
  <meta http-equiv="x-ua-compatible" content="ie=edge" />
  <title><xsl:value-of select="($locale/String[@Id='S_Splash_Title'])[1]" /></title>
  <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>menu.css</xsl:attribute></link>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>menu.js</xsl:attribute><xsl:text> </xsl:text></script>
<style type="text/css">
* { font-family: Tahoma; }
html, body { 
  padding: 0px;
  margin: 0px; 
  overflow: hidden; 
  font: 13.3px Tahoma;
}
div { position: static; }

#size {
  position: absolute; left: 0; top: 0; border: solid 1px #a53f20;
  background: white;
  width: 698px;
  height: 420px;
}
#caption {
  position: absolute;
  left: 0;
  top: 0;
  background: url("<xsl:value-of select="/Keyman/templatepath"/>banner-bg.png") #ad4a28;
  height: 63px;
  width: 698px;
}
#displaylanguage { top: 40px; right: 12px; position: absolute; color: white; }
#displaylanguage a { color: white; }

#titleBox { position: absolute; left: 23px; top: 80px; width: 651px; height: 59px; font: bold 26pt Tahoma }

#footer {
  left: 0;
  bottom: 0;
  position: absolute;
  width: 700px; 
  padding: 8px;
}

#evaluation { position: static; }
#buttons { float: right; position: static; text-align: right; }
#version { position: absolute; font-size: 0.8em; right: 90px; bottom: 12px; color: #888888 }
#beta { color: red; font-weight: bold;  }

#registered { height: 2px; }

#edition { position: absolute; right: 6px; top: 336px; font: bold 18.7px Tahoma; color: white; }

a.button {
  cursor: pointer;
  text-align: center;
  color: black;
  text-decoration: none;
  position: absolute;
}

a.button:hover { color: #ad4a28; }

.btn-grey-left { float: left; width: 3px; height: 34px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-grey-left.png"); }
.btn-grey { float: left; width: 324px; height: 26px; padding-top: 8px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-grey-mid.png") top left repeat-x; text-align: center; }
.btn-grey-right { float: left; width: 3px; height: 34px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-grey-right.png"); }

.btn-blue-left { float: left; width: 3px; height: 42px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-blue-left.png"); }
.btn-blue { float: left; width: 324px; height: 34px; padding-top: 8px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-blue-mid.png"); text-align: center; }
.btn-blue-right { float: left; width: 3px; height: 42px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-blue-right.png"); }

.btn-small-left { float: left; width: 2px; height: 23px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-small-left.png"); }
.btn-small { float: left; width: 61px; padding-top: 3px; height: 20px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-small-mid.png") top left repeat-x; }
.btn-small-right { float: left; width: 2px; height: 23px; background: url("<xsl:value-of select="/Keyman/templatepath"/>btn-small-right.png"); }

.highlighted .btn-grey { color: #0044ff; font-weight: bold; font-size: 16.0px; }

.disabled .btn-blue,
.disabled .btn-blue-left,
.disabled .btn-blue-right { filter: progid:DXImageTransform.Microsoft.BasicImage(grayScale=1); }

#img { left: 23px; top: 168px; position: absolute; background: url("<xsl:value-of select="/Keyman/templatepath"/>welcome_logo.png") no-repeat; display: block; width: 180px; height: 156px; }

#tasks { position: absolute; left: 0; bottom: 0; margin-left: 8px; margin-bottom: 8px; font-size: 11px }
#tasks div { margin-bottom: 2px; margin-left: 4px }

.label_disabled
{
  color: #888888;
}
  
.disabled,
.disabled:hover
{
  cursor: default !important;
  color: #888888 !important;
  background: #fcfcfc !important;
}


#learnHow, #startNow { left: 248px; }

#learnHow { top: 188px; }
#startNow { font-size: 20px; font-weight: bold; top: 240px; }
#exit { right: 12px; bottom: 12px; }

#tasks div#showAtStartupBox { margin-top: 6px; margin-left: 0; }
#tasks a { color: #9d3a18; text-decoration: none; }
#tasks a:hover { text-decoration: underline; }
#showAtStartup { margin: 0 4px 0 0; }
#tasksTitle { font-weight: bold; color: #303030; }

</style>
  <script type="text/javascript">
    function toggleDisplay(chk)
    {
      if(chk.checked) location.href = 'keyman:showsplash';
      else location.href = 'keyman:hidesplash';
      return false;
    }
    
    window.onload = function()
    {
      document.onkeydown = function()
      {
        switch(event.keyCode) {
          case 13:    // enter
            if( event.srcElement.id.substring(0,6) != 'button' ) {
              event.cancelBubble = true; event.returnValue = false;
              location.href='keyman:evaluate';
              break;
            }
            event.cancelBubble = true; event.returnValue = true;
            break;
          case 27:    // esc
            event.cancelBubble = true; event.returnValue = false;
            location.href='keyman:exit';
            break;
        }
      }
    }
    
  </script>
</head>

<body>
<div id="size">
  <div id="caption">
    <div id="displaylanguage">
      <xsl:value-of select="$locale/String[@Id='S_DisplayIn']" />:
      <a id="button_uilanguage" href="#" onclick="return false;"  onmousedown="ShowMenu('uilanguage','right'); return false;" onkeydown="if(event.keyCode == 32) ShowMenu('uilanguage','right'); else return true; return false;">
        <xsl:value-of select="$locale/String[@Id='SKUILanguageNameWithEnglish']"/>
      </a>
      <xsl:call-template name="popupmenu_uilanguage" />
    </div>
  </div>
  
  <div id="img"> </div>
  
  <div id="titleBox">
    Keyman Desktop for FirstVoices 9.0
  </div>
  
  <a href="keyman:tutorial" id="learnHow">
    <xsl:attribute name="class">button</xsl:attribute>
    <div class="btn-grey-left"></div><div class="btn-grey"><xsl:value-of select="$locale/String[@Id='S_Splash_LearnHow']"/></div><div class="btn-grey-right"></div>
  </a>
  <a href="keyman:evaluate" id="startNow">
    <xsl:attribute name="class">button</xsl:attribute>
    <div class="btn-blue-left"></div><div class="btn-blue"><xsl:value-of select="$locale/String[@Id='S_Splash_Start']"/></div><div class="btn-blue-right"></div>
  </a>
  <a href="keyman:exit" id="exit" class="button"><div class="btn-small-left"></div><div class="btn-small"><xsl:value-of select="$locale/String[@Id='S_Splash_Exit']"/></div><div class="btn-small-right"></div></a>

  <div id="tasks">
    <div><a href="keyman:config"><xsl:value-of select="$locale/String[@Id='S_Splash_Configuration']"/></a></div>
    <div id="showAtStartupBox">
      <input type="checkbox" id="showAtStartup" checked="checked" onclick="toggleDisplay(this)" />
      <label for="showAtStartup"><xsl:if test="/Keyman/Evaluation"><xsl:attribute name="class">label_disabled</xsl:attribute></xsl:if><xsl:value-of select="$locale/String[@Id='S_Splash_ShowAtStartupCheckbox']"/></label>
    </div>
  </div>
 
  <div id="version"><xsl:value-of select="/Keyman/Version"/>
    <span id="beta"><xsl:value-of select="$locale/String[@Id='S_BETA']"/></span>
  </div>
</div>
</body>
</html>
  </xsl:template>
  
  <xsl:template name="popupmenu_uilanguage">
    <div class="menu" id="menu_uilanguage">
      <xsl:for-each select="/Keyman/uilanguages/uilanguage">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="id">menu_uilanguage_<xsl:value-of select="@code" /></xsl:with-param>
          <xsl:with-param name="caption"><xsl:value-of select="@name" /></xsl:with-param>
          <xsl:with-param name="command">keyman:uilanguage?value=<xsl:value-of select="@code" /></xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
      <div class="menubreak">&#160;</div>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="id">menu_uilanguage_more</xsl:with-param>
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_MoreUILanguagesMenu']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:downloaduilanguages</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
  
</xsl:stylesheet>
