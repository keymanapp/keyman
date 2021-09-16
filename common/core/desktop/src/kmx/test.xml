<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl" />

  <xsl:variable name="dialoginfo_installkeyboard" select="$dialoginfo/Dialog[@Id='InstallKeyboard'][1]" />

  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
        <title><xsl:value-of select="$locale/string[@name='S_InstallKeyboard_Title']"/></title>
        <link rel="stylesheet" type="text/css" href="/app/config.css" />
        <link rel="stylesheet" type="text/css" href="/app/installkeyboard.css" />
        <style type="text/css">
          * {
            font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI";
          }

          #background {
            width: <xsl:value-of select="$dialoginfo_installkeyboard/@Width" />px;
            height: <xsl:value-of select="$dialoginfo_installkeyboard/@Height" />px;
          }
          #foreground {
            width: <xsl:value-of select="$dialoginfo_installkeyboard/@Width" />px;
            height: <xsl:value-of select="$dialoginfo_installkeyboard/@Height" />px;
          }

          #frame {
            width: <xsl:value-of select="$dialoginfo_installkeyboard/@Width - 20" />px;
          }

          #footer {
            width: <xsl:value-of select="$dialoginfo_installkeyboard/@Width - 16" />px;
          }
        </style>
        <script src="/app/installkeyboard.js"></script>
      </head>
      <body>
        <div id="background"></div>
        <div id="foreground">
          <div id="frame">
            <xsl:if test="/Keyman/KeymanPackageFile/readme">
              <xsl:attribute name="class">has_readme</xsl:attribute>
            </xsl:if>
            <div id="image">
              <img style="height: 250px; width: 140px;">
                <xsl:choose>
                  <xsl:when test="/Keyman/KeymanKeyboardFile">
                    <xsl:attribute name="src">/app/defaultkeyboard.gif</xsl:attribute>
                  </xsl:when>
                  <xsl:when test="/Keyman/KeymanPackageFile/graphic">
                    <xsl:attribute name="src">/data/installkeyboard/package/<xsl:value-of select="/Keyman/KeymanPackageFile/graphic" />?tag=<xsl:value-of select="/Keyman/PageTag"/></xsl:attribute>
                    <xsl:attribute name="style">height: 250px; width: 140px; border: solid 1px black</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="src">/app/defaultpackage.gif</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
              </img>
            </div>
            <xsl:if test="/Keyman/KeymanPackageFile/readme">
              <!-- add tabs to the dialog for details and readme -->
              <div id="tabs">
                <div id="tabDetails" onclick="javascript:tabClick(this)"><xsl:value-of select="$locale/string[@name='S_InstallKeyboard_Tab_Details']"/></div>
                <div id="tabReadme" onclick="javascript:tabClick(this)"><xsl:value-of select="$locale/string[@name='S_InstallKeyboard_Tab_Readme']"/></div>
              </div>
              <div class="content" id="readme">
                <iframe style="visibility: visible; width:446px; height:100%; padding: 0px; margins: 0px" scrolling="auto" frameborder="no" id="frameReadme">
                  <xsl:attribute name="src">/data/installkeyboard/package/<xsl:value-of select="/Keyman/KeymanPackageFile/readme" />?tag=<xsl:value-of select="/Keyman/PageTag"/></xsl:attribute>
                </iframe>
              </div>
            </xsl:if>
            <div class="content" id="details">
              <div style="display:block;position:static;margin:0;padding:0;background:blue;overflow:hidden;">
              <table style="width: 100%;background:white;">
                <tr>
                  <td colspan="2" class="keyboardname">
                    <xsl:choose>
                      <xsl:when test="//KeymanKeyboardFile/bitmap">
                        <img style="margin-right: 6px; float: left; height: 16px; width: 16px;">
                          <xsl:attribute name="src">/data/installkeyboard/bitmap/<xsl:value-of select="//KeymanKeyboardFile[1]/bitmap"/>?tag=<xsl:value-of select="/Keyman/PageTag"/></xsl:attribute>
                        </img>
                      </xsl:when>
                      <xsl:otherwise>
                        <img style="margin-right: 6px; float: left; height: 16px; width: 16px;" src="/app/no_icon.gif" />
                      </xsl:otherwise>
                    </xsl:choose>
                    <xsl:value-of select="/Keyman/KeymanKeyboardFile/name" />
                    <xsl:value-of select="/Keyman/KeymanPackageFile/name" />
                  </td>
                </tr>

                <xsl:apply-templates select="/Keyman/KeymanKeyboardFile" />
                <xsl:apply-templates select="/Keyman/KeymanPackageFile" />
              </table>
              </div>
            </div>
          </div>
          <div id="footer">
						<xsl:if test="/Keyman/canelevate">
							<xsl:call-template name="button">
								<xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_InstallKeyboard_Button_Install']"/></xsl:with-param>
								<xsl:with-param name="command">javascript:keyboard_install(true)</xsl:with-param>
								<xsl:with-param name="width">120px</xsl:with-param>
                <xsl:with-param name="shield">1</xsl:with-param>
							</xsl:call-template>
						</xsl:if>
            <xsl:if test="/Keyman/isadmin">
              <xsl:call-template name="button">
                <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_InstallKeyboard_Button_Install']"/></xsl:with-param>
                <xsl:with-param name="default">1</xsl:with-param>
                <xsl:with-param name="command">javascript:keyboard_install(false)</xsl:with-param>
                <xsl:with-param name="width">70px</xsl:with-param>
              </xsl:call-template>
            </xsl:if>
            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Cancel']"/></xsl:with-param>
              <xsl:with-param name="command">keyman:keyboard_cancel</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="KeymanKeyboardLanguage">
    <option>
      <xsl:attribute name="value"><xsl:value-of select="bcp47code" /></xsl:attribute>
      <xsl:if test="bcp47code = /Keyman/DefaultBCP47Tag"><xsl:attribute name="selected">selected</xsl:attribute></xsl:if>
      <xsl:value-of select="langname" />
    </option>
  </xsl:template>

  <xsl:template mode="language-picker" match="/Keyman/KeymanPackageFile/KeymanPackageContentKeyboardsFile/KeymanKeyboardFile">
    <select class="keyboardLanguage">
      <xsl:attribute name="id">keyboardLanguage_<xsl:value-of select="id"/></xsl:attribute>
      <xsl:apply-templates select="KeymanKeyboardLanguagesFile/KeymanKeyboardLanguage">
        <xsl:sort select="langname" />
      </xsl:apply-templates>
    </select>
  </xsl:template>

  <xsl:template mode="keyboard-name" match="/Keyman/KeymanPackageFile/KeymanPackageContentKeyboardsFile/KeymanKeyboardFile">
    <xsl:value-of select="name"/><br />
  </xsl:template>

  <xsl:template match="/Keyman/KeymanPackageFile/Fonts/Font/name">
    <xsl:value-of select="."/><br />
  </xsl:template>

  <xsl:template match="/Keyman/KeymanKeyboardFile">
    <tr>
      <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Filename']"/></td>
      <td class="otherdetails"><xsl:value-of select="filename" /></td>
    </tr>

    <tr>
      <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Encodings']"/></td>
      <td class="otherdetails">
        <xsl:for-each select="encodings/encoding">
          <xsl:value-of select="."/>
          <xsl:if test="not(last())">,</xsl:if>
        </xsl:for-each>
      </td>
    </tr>

    <xsl:if test="copyright">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Copyright']"/></td>
        <td class="otherdetails"><xsl:value-of select="copyright" /></td>
      </tr>
    </xsl:if>

    <xsl:if test="message">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Message']"/></td>
        <td class="otherdetails"><xsl:value-of select="message" /></td>
      </tr>
    </xsl:if>
  </xsl:template>

  <xsl:template match="/Keyman/KeymanPackageFile">
    <xsl:choose>
      <xsl:when test="count(KeymanPackageContentKeyboardsFile/KeymanKeyboardFile) = 1">
        <tr>
          <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Keyboard']"/></td>
          <td class="otherdetails"><xsl:apply-templates mode="keyboard-name" select="KeymanPackageContentKeyboardsFile/KeymanKeyboardFile" /></td>
        </tr>
        <xsl:if test="count(KeymanPackageContentKeyboardsFile/KeymanKeyboardFile/KeymanKeyboardLanguagesFile/KeymanKeyboardLanguage) > 1">
          <!-- This test presents the selection for keyboard language only for packages with a single keyboard and 
               more than one language option. In future, we could consider extending it for packages with more than 
               one keyboard, but that would take more plumbing for implementation. -->
          <tr>
            <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_KeyboardLanguage']"/></td>
            <td class="otherdetails"><xsl:apply-templates mode="language-picker" select="KeymanPackageContentKeyboardsFile/KeymanKeyboardFile" /></td>
          </tr>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <tr>
          <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Keyboards']"/></td>
          <td class="otherdetails"><xsl:apply-templates mode="keyboard-name" select="KeymanPackageContentKeyboardsFile/KeymanKeyboardFile" /></td>
        </tr>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:if test="Fonts/Font">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/string[@name='S_Caption_Fonts']"/></td>
        <td class="otherdetails"><xsl:apply-templates select="Fonts/Font/name" /></td>
      </tr>
    </xsl:if>

    <xsl:apply-templates select="detail[@name='version']"><xsl:with-param name="header"><xsl:value-of select="$locale/string[@name='S_Caption_Version']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='author']"><xsl:with-param name="header"><xsl:value-of select="$locale/string[@name='S_Caption_Author']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='website']"><xsl:with-param name="header"><xsl:value-of select="$locale/string[@name='S_Caption_Website']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='copyright']"><xsl:with-param name="header"><xsl:value-of select="$locale/string[@name='S_Caption_Copyright']"/></xsl:with-param></xsl:apply-templates>

  </xsl:template>

  <xsl:template match="detail">
    <xsl:param name="header" />
    <tr>
      <td class="detailheader"><xsl:value-of select="$header" /></td>
      <td class="otherdetails">
        <xsl:choose>
          <xsl:when test="@url">
            <a>
              <xsl:attribute name="href">keyman:link?url=<xsl:value-of select="@url"/></xsl:attribute>
              <xsl:attribute name="title"><xsl:value-of select="@url"/></xsl:attribute>
              <xsl:value-of select="." />
            </a>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="." />
          </xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>
  </xsl:template>

</xsl:stylesheet>