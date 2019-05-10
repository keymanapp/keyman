<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:include href="elements.xsl"/>

  <xsl:variable name="locale_keyman" select="$locale/Dialog[@Id='Keyman']" />
  
  <xsl:include href="keyman_menu.xsl"/>

  <xsl:include href="keyman_keyboardlist.xsl"/>
  <xsl:include href="keyman_options.xsl"/>
  <xsl:include href="keyman_hotkeys.xsl"/>
  <xsl:include href="keyman_support.xsl"/>

  <xsl:include href="keyman_footer.xsl"/>
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <title><xsl:value-of select="$locale/String[@Id='S_ConfigurationTitle']"/></title>
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>config.css</xsl:attribute></link>
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>menu.css</xsl:attribute></link>
        <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>config.js</xsl:attribute><xsl:text> </xsl:text></script>
        <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>menu.js</xsl:attribute><xsl:text> </xsl:text></script>
        <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>menu-frame.js</xsl:attribute><xsl:text> </xsl:text></script>
      </head>      
      
      <body visited="#0000FF">
        
        <div style="position: absolute; width: 100%; height: 100%; z-index: 10; top: 0">
          <div id="titleframe">
            <div id="displaylanguage">
              <xsl:value-of select="$locale/String[@Id='S_DisplayIn']" />:
              <a id="button_uilanguage" href="#" onmousedown="ShowMenu('uilanguage','right'); return false;" onkeydown="if(event.keyCode == 32) ShowMenu('uilanguage','right'); else return true; return false;"
              ><xsl:value-of select="$locale/String[@Id='SKUILanguageNameWithEnglish']"/></a>
            <xsl:call-template name="popupmenu_uilanguage"></xsl:call-template>
            </div>
          </div>
          <div id="header-pad"></div>
          <div id="menuframe" tabindex="-1">
            <div id="state"><xsl:value-of select="state"/></div>
            <xsl:call-template name="menuframe" />
            <xsl:if test="/Keyman[state!='']">
              <script type='text/javascript'>
                menuframe_activeindex = <xsl:value-of select="/Keyman/state" />;
              </script>
            </xsl:if>
          </div>
          <div id="contentframe">
            <div class="contentpage" id="content_keyboardlist"><xsl:call-template name="content_keyboards" /></div>
            <div class="contentpage" id="content_options"><xsl:call-template name="content_options" /></div>
            <div class="contentpage" id="content_hotkeys"><xsl:call-template name="content_hotkeys" /></div>
            <div class="contentpage" id="content_support"><xsl:call-template name="content_support" /></div>
          </div>
          <div id="footerframe">
            <xsl:call-template name="footerframe" />
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template name="header_helplinks">
    <div class="helplinks">
      <a href="keyman:welcome" onmouseover="this.style.cursor='hand';" style="padding-right: 12px">
        <img onmouseover="this.style.cursor='hand';" style='width: 24px; height: 24px; border: none; vertical-align: middle; margin: 0 4px 0 0;'>
          <xsl:attribute name='src'><xsl:value-of select='/Keyman/templatepath'/>info24.png</xsl:attribute>
        </img>
        <xsl:value-of select="$locale/String[@Id='S_Caption_GettingStarted']" />
      </a>
      <a href="keyman:help" onmouseover="this.style.cursor='hand';" >
        <img onmouseover="this.style.cursor='hand';" style='width: 24px; height: 24px; border: none; vertical-align: middle; margin: 0 4px 0 4px;'>
          <xsl:attribute name='src'><xsl:value-of select='/Keyman/templatepath'/>help24.png</xsl:attribute>
        </img>
        <xsl:value-of select="$locale/String[@Id='S_Caption_Help']" />
      </a>
    </div>
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
      <div class="menubreak" ></div>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="id">menu_uilanguage_more</xsl:with-param>
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_MoreUILanguagesMenu']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:downloaduilanguages</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
  
</xsl:stylesheet>