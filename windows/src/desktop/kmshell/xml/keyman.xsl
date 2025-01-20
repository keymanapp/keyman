<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>

  <xsl:variable name="dialoginfo_keyman" select="$dialoginfo/Dialog[@Id='Keyman']" />

  <xsl:include href="keyman_menu.xsl"/>

  <xsl:include href="keyman_keyboardlist.xsl"/>
  <xsl:include href="keyman_options.xsl"/>
  <xsl:include href="keyman_hotkeys.xsl"/>
  <xsl:include href="keyman_support.xsl"/>
  <xsl:include href="keyman_update.xsl"/>

  <xsl:include href="keyman_footer.xsl"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
        <style> *{font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI";}</style>
        <title><xsl:value-of select="$locale/string[@name='S_ConfigurationTitle']"/></title>
        <link rel="stylesheet" type="text/css" href="/app/config.css" />
        <link rel="stylesheet" type="text/css" href="/app/menu.css" />
        <script type="text/javascript" src="/app/jquery.min.js"></script>
        <script type="text/javascript" src="/app/config.js"></script>
        <script type="text/javascript" src="/app/menu.js"></script>
        <script type="text/javascript" src="/app/menu-frame.js"></script>
        <script type="text/javascript" src="/app/qrcode.min.js"></script>
      </head>

      <body visited="white">

        <div style="position: absolute; left: 0; top: 0; width: 100%; height: 100%; z-index: 10">
          <div id="header-pad"></div>
          <div id="menubackground" onmousedown="HideMenu()"></div>
          <div id="menuframe" tabindex="-1">
            <div id="state"><xsl:value-of select="/Keyman/state"/></div>
            <xsl:call-template name="menuframe" />
          </div>
          <div id="displaylanguage">
            <xsl:call-template name="popupmenu_uilanguage"></xsl:call-template>
          </div>
          <div id="contentframe">
            <div class="contentpage" id="content_keyboardlist"><xsl:call-template name="content_keyboards" /></div>
            <div class="contentpage" id="content_options"><xsl:call-template name="content_options" /></div>
            <div class="contentpage" id="content_hotkeys"><xsl:call-template name="content_hotkeys" /></div>
            <div class="contentpage" id="content_support"><xsl:call-template name="content_support" /></div>
            <div class="contentpage" id="content_update"><xsl:call-template name="content_update" /></div>
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
      <a href="keyman:help" onmouseover="this.style.cursor='hand';" >
        <img onmouseover="this.style.cursor='hand';" style='width: 24px; height: 24px; border: none; vertical-align: middle; margin: -2px 4px 2px 4px;' src="/app/help24.png" />
        <xsl:value-of select="$locale/string[@name='S_Caption_Help']" />
      </a>&#160;
      <img onmouseover="this.style.cursor='hand';" style='width: 24px; height: 24px; border: none; vertical-align: middle; margin: -2px 4px 2px 4px;' src="/app/globe.png" />
      <xsl:value-of select="$locale/string[@name='S_DisplayIn']" />:
      <a id="button_uilanguage" href="#"
          onmousedown="ShowMenu('uilanguage','right', document.body.offsetWidth, 38); return false;"
          onkeydown="if(event.keyCode == 32) ShowMenu('uilanguage','right'); else return true; return false;"
      ><xsl:value-of select="$locale/string[@name='SKUILanguageNameWithEnglish']"/></a>
    </div>
  </xsl:template>

  <xsl:template name="popupmenu_uilanguage">
    <div class="menu" id="menu_uilanguage">
      <xsl:for-each select="/Keyman/uilanguages/uilanguage">
        <xsl:sort select="@name" />
        <xsl:call-template name="menuitem">
          <xsl:with-param name="id">menu_uilanguage_<xsl:value-of select="@code" /></xsl:with-param>
          <xsl:with-param name="caption"><xsl:value-of select="@name" /></xsl:with-param>
          <xsl:with-param name="command">keyman:uilanguage?value=<xsl:value-of select="@code" /></xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
      <div class="menubreak" ></div>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="id">menu_uilanguage_contribute</xsl:with-param>
        <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_ContributeUILanguagesMenu']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:contributeuilanguages</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
</xsl:stylesheet>
