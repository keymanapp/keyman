<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <xsl:template name="content_keyboards">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
      </script>
    ]]></xsl:text>

    <div class="header" id="keyboards_header">
  		<xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/string[@name='S_Keyboards']"/>
    </div>

    <div class="content" id="subcontent_keyboards">
      <div class="contentBox" id='keyboards-content-box'>
        <xsl:choose>
          <xsl:when test="Keyman/KeymanKeyboardInstalled or Keyman/KeymanPackageInstalled">
            <div id="keyboards" class="list">
              <xsl:for-each select="Keyman/KeymanKeyboardInstalled | Keyman/KeymanPackageInstalled/KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[count(../KeymanKeyboardInstalled)=1]">
                <xsl:sort select="name" />
                <xsl:choose>
                  <xsl:when test="name(..) = 'KeymanPackageContentKeyboardsInstalled'">
                    <xsl:call-template name="keyboard">
                      <xsl:with-param name="singlekeyboardpackage">1</xsl:with-param>
                    </xsl:call-template>
                  </xsl:when>
                  <xsl:otherwise><xsl:call-template name="keyboard" /></xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>

              <xsl:for-each select="Keyman/KeymanPackageInstalled[count(./KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled)!=1]">
                <xsl:sort select="name" />

                  <div class="item">
                    <xsl:attribute name="id">package_<xsl:value-of select="id" /></xsl:attribute>

                    <img class="list_icon" src="/app/package.gif" />
                    <div class="packagetitle">
                      <xsl:choose>
                        <xsl:when test="name"><xsl:value-of select="name"/></xsl:when>
                        <xsl:otherwise><xsl:value-of select="id"/>.kmp</xsl:otherwise>
                      </xsl:choose>
                    </div>
                    <br style="clear:left" />
                  </div>
                  <xsl:for-each select="KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled">
                    <xsl:sort select="name" />
                    <xsl:call-template name="keyboard" />
                  </xsl:for-each>
              </xsl:for-each>
            </div>
          </xsl:when>
          <xsl:otherwise>
            <div id="keyboards" class='info'>
              <div>
                <img src="/app/info.png" />
                <xsl:value-of select="$locale/string[@name='S_Keyboards_NoKeyboardsInstalled']"/>
              </div>
            </div>
          </xsl:otherwise>
        </xsl:choose>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="keyboard">
    <xsl:param name="singlekeyboardpackage" />
    <xsl:variable name="id"><xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="id" />
      <xsl:with-param name="replace" select='"&apos;"' />
      <xsl:with-param name="with" select='"\&apos;"' />
    </xsl:call-template></xsl:variable>

    <xsl:variable name="package_id"><xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="../../id"/>
      <xsl:with-param name="replace" select='"&apos;"' />
      <xsl:with-param name="with" select='"\&apos;"' />
    </xsl:call-template></xsl:variable>

    <div tabindex="1" tagType="listitem">
      <xsl:attribute name="class">list_item</xsl:attribute>
      <xsl:attribute name="id">list_keyboard_<xsl:value-of select="id"/></xsl:attribute>
      <xsl:attribute name="data-name"><xsl:value-of select="name"/></xsl:attribute>

			<xsl:attribute name="onkeydown">return list_keydown(event,'keyboard_<xsl:value-of select="$id"/>');</xsl:attribute>
      <xsl:attribute name="onfocus">return list_focus(event,'keyboard_<xsl:value-of select="$id"/>');</xsl:attribute>
			<xsl:attribute name="onblur">return list_blur(event,'keyboard_<xsl:value-of select="$id"/>');</xsl:attribute>
			<xsl:attribute name="oncontextmenu">event.cancelBubble=true;event.returnValue=false;</xsl:attribute>

      <xsl:attribute name="style">left:0;top:0;width:99%</xsl:attribute>
      <div>
        <xsl:attribute name="onmousedown">return list_detail(event,'keyboard_<xsl:value-of select="$id"/>');</xsl:attribute>
        <xsl:attribute name="id">listtitle_keyboard_<xsl:value-of select="id"/></xsl:attribute>
        <xsl:attribute name='class'>
          list_title <xsl:choose><xsl:when test='loaded'>keyboard_loaded</xsl:when><xsl:otherwise>keyboard_unloaded</xsl:otherwise></xsl:choose>
        </xsl:attribute>
        <div style="float:left; margin-top: 3px;">
          <div class="check_box_invisible">
          <input type='checkbox'>
            <xsl:attribute name="id">keyboardcheck_<xsl:value-of select="id"/></xsl:attribute>
            <xsl:attribute name='onmousedown'>event.stopPropagation();return false;</xsl:attribute>
            <xsl:attribute name='onclick'>return keyboard_checkclick('<xsl:value-of select="$id"/>');</xsl:attribute>
            <xsl:if test='loaded'><xsl:attribute name='checked'>checked</xsl:attribute></xsl:if>
          </input>
          </div>
          <div class="list_icon">
            <img style="width: 16px; height:16px;">
              <xsl:choose>
                <xsl:when test="bitmap"><xsl:attribute name="src">/data/keyman/bitmap/<xsl:value-of select="bitmap"/>?tag=<xsl:value-of select="/Keyman/PageTag" /></xsl:attribute></xsl:when>
                <xsl:otherwise><xsl:attribute name="src">/app/no_icon.gif</xsl:attribute></xsl:otherwise>
              </xsl:choose>
            </img>
          </div>
          <xsl:text> </xsl:text>
          <div class="keyboard_header" style="display: inline">
            <xsl:attribute name="id">keyboard_name_<xsl:value-of select="id"/></xsl:attribute>
            <xsl:choose>
              <xsl:when test="name"><xsl:value-of select="name"/></xsl:when>
              <xsl:otherwise><xsl:value-of select="id"/>.kmx</xsl:otherwise>
            </xsl:choose>
            <xsl:text> </xsl:text>
          </div>
        </div>

        <div style="float:right">
          <div style="float: left; padding: 1px 3px 1px 0px;">
            <a class="hotkey" tabindex="-1">
              <xsl:attribute name="href">keyman:hotkey_set?index=hotkey_lang_<xsl:value-of select="position()-1"/></xsl:attribute>
              <xsl:attribute name="onmouseover">this.style.cursor='hand';</xsl:attribute>
              <xsl:choose>
                <xsl:when test="hotkey"><xsl:value-of select="hotkey"/></xsl:when>
                <xsl:otherwise><xsl:value-of select="$locale/string[@name='S_Hotkey_None']"/></xsl:otherwise>
              </xsl:choose>
            </a>
          </div>
        </div>


        <div style="clear:both"></div>
      </div>
        <div class="list_detail">
          <xsl:attribute name="id">list_detail_keyboard_<xsl:value-of select="id"/></xsl:attribute>
          <div class="flex-container">

          <div class="grid-container grid_rows_hide">
            <xsl:attribute name="id">keyboard_grid_<xsl:value-of select="id"/></xsl:attribute>

            <xsl:if test="version">
              <div class="grid_item">
                <xsl:value-of select="$locale/string[@name='S_Caption_KeyboardVersion']"/>
              </div>
              <div class="grid_item">
                <xsl:value-of select="version"/>
              </div>
            </xsl:if>
            <div class="grid_item">
              <xsl:value-of select="$locale/string[@name='S_Caption_Languages']"/>
            </div>
            <div class="grid_item">
                <xsl:apply-templates select="KeymanKeyboardLanguagesInstalled/KeymanKeyboardLanguageInstalled[isinstalled]" />
                <div class='list-languages-add'>
                  <xsl:call-template name="button">
                    <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Languages_Install']"/></xsl:with-param>
                    <xsl:with-param name="command">keyman:keyboardlanguage_install?id=<xsl:value-of select="id"/></xsl:with-param>
                  </xsl:call-template>
                </div>
            </div>

            <!--- The following rows will have the data-hide attribute -->

            <xsl:if test="id">
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Filename']"/>
              </div>
              <div class="grid_item" data-hide="">
                <xsl:value-of select="id"/>.kmx
              </div>
            </xsl:if>
            <xsl:if test="$singlekeyboardpackage = '1'">
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Package']"/>
              </div>
              <div class="grid_item" data-hide="">
                <xsl:value-of select="../../name"/>
              </div>
            </xsl:if>


            <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]">
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Version']"/>
              </div>
              <div class="grid_item" data-hide="">
                <xsl:value-of select="../../detail[@name='version']"/>
              </div>
            </xsl:if>

            <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]/../../Fonts"><!-- I2216 -->
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Fonts']" />
              </div>
              <div class="grid_item" data-hide="">
                <xsl:for-each select="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]/../../Fonts/Font">
                  <xsl:value-of select="name"/>
                  <xsl:if test="position() != last()">, </xsl:if>
                </xsl:for-each>
              </div>
            </xsl:if>
            <xsl:if test="encodings">
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Encodings']"/>
              </div>
              <div class="grid_item" data-hide="">
                  <xsl:for-each select="encodings/encoding">
                    <xsl:value-of select="."/>
                    <xsl:if test="position() != last()">, </xsl:if>
                  </xsl:for-each>
              </div>
            </xsl:if>
            <div class="grid_item" data-hide="">
              <xsl:value-of select="$locale/string[@name='S_Caption_LayoutType']"/>
            </div>
            <div class="grid_item" data-hide="">
              <xsl:choose>
                <xsl:when test="layoutpositional"><xsl:value-of select="$locale/string[@name='S_LayoutType_Positional']"/></xsl:when>
                <xsl:otherwise><xsl:value-of select="$locale/string[@name='S_LayoutType_Mnemonic']"/></xsl:otherwise>
              </xsl:choose>
            </div>

            <div class="grid_item" data-hide="">
              <xsl:value-of select="$locale/string[@name='S_Caption_OnScreenKeyboard']"/>
            </div>
            <div class="grid_item" data-hide="">
              <xsl:choose>
                <xsl:when test="../../usage"><xsl:value-of select="$locale/string[@name='S_OnScreenKeyboard_Custom']"/></xsl:when>
                <xsl:when test="visualkeyboard"><xsl:value-of select="$locale/string[@name='S_OnScreenKeyboard_Installed']"/></xsl:when>
                <xsl:otherwise><xsl:value-of select="$locale/string[@name='S_OnScreenKeyboard_NotInstalled']"/></xsl:otherwise>
              </xsl:choose>
            </div>
            <div class="grid_item" data-hide="">
              <xsl:value-of select="$locale/string[@name='S_Caption_Documentation']"/>
            </div>
            <div class="grid_item" data-hide="">
              <xsl:choose>
                <xsl:when test="../../welcome"><xsl:value-of select="$locale/string[@name='S_Documentation_Installed']"/></xsl:when>
                <xsl:otherwise><xsl:value-of select="$locale/string[@name='S_Documentation_NotInstalled']"/></xsl:otherwise>
              </xsl:choose>
            </div>

            <xsl:if test="message">
              <div class="grid_item" data-hide="">
                <xsl:value-of select="$locale/string[@name='S_Caption_Message']"/>
              </div>
              <div class="grid_item" data-hide="">
                  <xsl:value-of select="message"/>
              </div>
            </xsl:if>
            <xsl:if test="copyright">
              <div class="grid_item" data-hide="">
                  <xsl:value-of select="$locale/string[@name='S_Caption_Copyright']"/>
              </div>
              <div class="grid_item" data-hide="">
                  <xsl:value-of select="copyright"/>
              </div>
            </xsl:if>

            <div class="grid_item">
              <xsl:attribute name='onclick'>return more_detail('keyboard_grid_<xsl:value-of select="$id"/>');</xsl:attribute>
              More
            </div>

            <div class="grid_col_span_2 grid_item">

              <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]">
                <xsl:call-template name="button">
                  <xsl:with-param name="caption">Help</xsl:with-param>
                  <xsl:with-param name="command">keyman:package_welcome?id=<xsl:value-of select="$package_id" /></xsl:with-param>
                </xsl:call-template>
              </xsl:if>

            <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id] and ../../options">
              <xsl:call-template name="button">
                <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Menu_Options']"/></xsl:with-param>
                <xsl:with-param name="command">keyman:keyboard_options?id=<xsl:value-of select="id"/></xsl:with-param>
              </xsl:call-template>
            </xsl:if>

              <xsl:call-template name="button">
                <xsl:with-param name="caption">Disable</xsl:with-param>
                <xsl:with-param name="id"><xsl:value-of select="id"/></xsl:with-param>
                <xsl:with-param name="onclick">return keyboard_toggle('<xsl:value-of select="id"/>')</xsl:with-param>
              </xsl:call-template>
              <xsl:choose>
                <xsl:when test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]">
                  <xsl:call-template name="button">
                    <xsl:with-param name="caption">Uninstall</xsl:with-param>
                    <xsl:with-param name="command">keyman:package_uninstall?id=<xsl:value-of select="$package_id"/></xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:call-template name="button">
                    <xsl:with-param name="caption">Uninstall</xsl:with-param>
                    <xsl:with-param name="command">keyman:keyboard_uninstall?id=<xsl:value-of select="$id"/></xsl:with-param>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </div>

          </div> <!---end keyboard details grid container -->

                      <!--- comment insert QR code here -->
          <div class="qrcode flex-item-qr">
            <xsl:attribute name='id'>qrcode-<xsl:value-of select="../../id" /></xsl:attribute>
              <div>
                <xsl:attribute name='id'>qrcode-img-<xsl:value-of select="../../id" /></xsl:attribute>
              </div>
              <div class='qrcode-caption'>
                <xsl:value-of select="$locale/string[@name='S_Keyboard_Share_QRCode']"/>
                <a>
                  <xsl:attribute name="href">keyman:link?url=<xsl:value-of select="/Keyman/keyman-com" />/go/keyboard/<xsl:value-of select="../../id" />/share  </xsl:attribute>
                  &#x0020;
                  <xsl:value-of select="$locale/string[@name='S_Keyboard_Share_Link']"/>
                </a>
                <xsl:value-of select="$locale/string[@name='S_Keyboard_Share_LinkSuffix']"/>
              </div>
              <script>new QRCode(document.getElementById("qrcode-img-<xsl:value-of select="../../id"/>"), {
                text: '<xsl:value-of select="/Keyman/keyman-com" />/go/keyboard/<xsl:value-of select="../../id"/>/share',
                width: 128,
                height: 128
              });
            </script>
          </div>
          </div>



                      <!--- end qr commment -->
          <div style="clear:both">&#160;</div>

        </div>
      </div>

  </xsl:template>

  <xsl:template match="KeymanKeyboardLanguageInstalled">
    <div class='keyboard-language'>
      <a>
        <xsl:attribute name="href">keyman:keyboardlanguage_uninstall?id=<xsl:value-of select='../../id'/>&amp;bcp47code=<xsl:value-of select="bcp47code"/></xsl:attribute>
        <img src="/app/cross.png">
          <xsl:attribute name="title"><xsl:value-of select="$locale/string[@name='S_Languages_Uninstall']"/></xsl:attribute>
        </img>
      </a>
      <span class="language-icon"><xsl:value-of select="substring(bcp47code, 1, 2)" /></span>
      <span><xsl:value-of select="langname" /> (<xsl:value-of select="bcp47code" />)</span>
    </div>
  </xsl:template>

  </xsl:stylesheet>
