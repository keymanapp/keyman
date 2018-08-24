<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:template name="content_keyboards">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
      </script>
    ]]></xsl:text>

    <div class="header" id="keyboards_header">
  		<xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Keyboards']"/>
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
                      
                    <img class="list_icon">
                      <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>package.gif</xsl:attribute>
                    </img>
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
                <img>
                  <xsl:attribute name="src">
                    <xsl:value-of select="/Keyman/templatepath"/>info.png
                  </xsl:attribute>
                </img>
                <xsl:value-of select="$locale/String[@Id='S_Keyboards_NoKeyboardsInstalled']"/>
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
          <input type='checkbox'>
            <xsl:attribute name="id">keyboardcheck_<xsl:value-of select="id"/></xsl:attribute>
            <xsl:attribute name='onmousedown'>event.stopPropagation();return false;</xsl:attribute>
            <xsl:attribute name='onclick'>return keyboard_checkclick('<xsl:value-of select="$id"/>');</xsl:attribute>
            <xsl:if test='loaded'><xsl:attribute name='checked'>checked</xsl:attribute></xsl:if>
          </input>
          <div class="list_icon">
            <img style="width: 16px; height:16px;">
              <xsl:choose>
                <xsl:when test="bitmap">          
                  <xsl:attribute name="src">
                    <xsl:value-of select="/Keyman/ImagePath"/>
                    <xsl:value-of select="bitmap"/>
                  </xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:attribute name="src">
                    <xsl:value-of select="/Keyman/templatepath"/>no_icon.gif
                  </xsl:attribute>
                </xsl:otherwise>
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

       
        <div class="list_expand">
          <xsl:attribute name="id">list_expand_keyboard_<xsl:value-of select="id"/></xsl:attribute>
        </div>
       
        <xsl:choose>
          <xsl:when test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]">
            <div class="list_close">
              <xsl:attribute name="id">list_close_keyboard_<xsl:value-of select="id"/></xsl:attribute>
              <xsl:attribute name="onmousedown">location.href='keyman:package_uninstall?id=<xsl:value-of select="$package_id"/>';event.cancelBubble=true;return false;</xsl:attribute>
            </div>
            <div class="list_help">
              <xsl:attribute name="id">list_help_keyboard_<xsl:value-of select="id"/></xsl:attribute>
              <xsl:attribute name="onmousedown">location.href='keyman:package_welcome?id=<xsl:value-of select="$package_id" />';event.cancelBubble=true;return false;</xsl:attribute>
            </div>
          </xsl:when>
          <xsl:otherwise>
            <div class="list_close">
              <xsl:attribute name="id">list_close_keyboard_<xsl:value-of select="id"/></xsl:attribute>
              <xsl:attribute name="onmousedown">location.href='keyman:keyboard_uninstall?id=<xsl:value-of select="$id"/>';event.cancelBubble=true;return false;</xsl:attribute>
            </div>
          </xsl:otherwise>
        </xsl:choose>

        <div style="clear:both"></div>
      </div>
        <div class="list_detail">
          <xsl:attribute name="id">list_detail_keyboard_<xsl:value-of select="id"/></xsl:attribute>
          <table>
            <tbody>
              <tr>
                <td>
                  <div class="list-languages">
                    <div class="list-languages-title"><xsl:value-of select="$locale/String[@Id='S_Caption_Languages']"/></div>
                    <div>
                      <xsl:apply-templates select="KeymanKeyboardLanguagesInstalled/KeymanKeyboardLanguageInstalled[isinstalled]" />
                      <div class='list-languages-add'>
                        <xsl:call-template name="button">
                          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Languages_Install']"/></xsl:with-param>
                          <xsl:with-param name="command">keyman:keyboardlanguage_install?id=<xsl:value-of select="id"/></xsl:with-param>
                        </xsl:call-template>
                      </div>
                    </div>
                  </div>
                </td>
                <td>
                  <div class="keyboard_options">
                    <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id] and ../../options">
                      <a><xsl:attribute name="href">keyman:keyboard_options?id=<xsl:value-of select="id" /></xsl:attribute><div class="keyboard_options_link"><xsl:value-of select="$locale/String[@Id='S_Menu_Options']"/></div></a>
                    </xsl:if>
                  </div>
                  <div class="keyboard_table">
                    <table style="font-size: 11px;">
                      <xsl:if test="id">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Filename']"/></td>
                          <td>
                          <span><xsl:value-of select="id"/>.kmx</span>
                        </td></tr>
                      </xsl:if>
                      <xsl:if test="version">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_KeyboardVersion']"/></td>
                          <td>
                          <span><xsl:value-of select="version"/></span>
                        </td></tr>
                      </xsl:if>
                      <xsl:if test="$singlekeyboardpackage = '1'">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Package']"/></td>
                          <td>
                            <span><xsl:value-of select="../../name"/></span>
                          </td>
                        </tr>    
                      </xsl:if>
                      <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Version']"/></td>
                          <td>
                            <span><xsl:value-of select="../../detail[@name='version']"/></span>
                          </td>
                        </tr>    
                      </xsl:if>
                      <xsl:if test="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]/../../Fonts"><!-- I2216 -->
                        <tr>
                          <td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Fonts']" /></td>
                          <td>
                            <xsl:for-each select="//KeymanPackageContentKeyboardsInstalled/KeymanKeyboardInstalled[id=current()/id]/../../Fonts/Font">
                              <xsl:value-of select="name"/>
                              <xsl:if test="position() != last()">, </xsl:if>
                            </xsl:for-each>
                          </td>
                        </tr>
                      </xsl:if>
                      <xsl:if test="encodings">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Encodings']"/></td>
                          <td>
                            <xsl:for-each select="encodings/encoding">
                              <xsl:value-of select="."/>
                              <xsl:if test="position() != last()">, </xsl:if>
                            </xsl:for-each>
                        </td></tr>
                      </xsl:if>
                      <tr>
                        <td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_LayoutType']"/></td>
                        <td>
                          <xsl:choose>
                            <xsl:when test="layoutpositional"><xsl:value-of select="$locale/String[@Id='S_LayoutType_Positional']"/></xsl:when>
                            <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_LayoutType_Mnemonic']"/></xsl:otherwise>
                          </xsl:choose>
                        </td>
                      </tr>
                      <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_OnScreenKeyboard']"/></td>
                        <td>
                          <xsl:choose>
                            <xsl:when test="../../usage"><xsl:value-of select="$locale/String[@Id='S_OnScreenKeyboard_Custom']"/></xsl:when>
                            <xsl:when test="visualkeyboard"><xsl:value-of select="$locale/String[@Id='S_OnScreenKeyboard_Installed']"/></xsl:when>
                            <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_OnScreenKeyboard_NotInstalled']"/></xsl:otherwise>
                          </xsl:choose>
                      </td></tr>
                        <tr>
                          <td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Documentation']"/></td>
                          <td>
                          <xsl:choose>
                            <xsl:when test="../../welcome"><xsl:value-of select="$locale/String[@Id='S_Documentation_Installed']"/></xsl:when>
                            <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_Documentation_NotInstalled']"/></xsl:otherwise>
                          </xsl:choose>
                          </td>
                        </tr>
                      <xsl:if test="message">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Message']"/></td>
                          <td>
                            <xsl:value-of select="message"/>
                        </td></tr>
                      </xsl:if>
                      <xsl:if test="copyright">
                        <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Copyright']"/></td>
                          <td>
                            <xsl:value-of select="copyright"/>
                        </td></tr>
                      </xsl:if>
                    </table>
                  </div>
                  <div style="clear:both">&#160;</div>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    
  </xsl:template>

  <xsl:template match="KeymanKeyboardLanguageInstalled">
    <div class='keyboard-language'>
      <a>
        <xsl:attribute name="href">keyman:keyboardlanguage_uninstall?id=<xsl:value-of select='../../id'/>&amp;index=<xsl:value-of select="position()-1"/></xsl:attribute>
        <img>
          <xsl:attribute name="title"><xsl:value-of select="$locale/String[@Id='S_Languages_Uninstall']"/></xsl:attribute>
          <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>cross.png</xsl:attribute>
        </img>
      </a>
      <span class="language-icon"><xsl:value-of select="substring(bcp47code, 1, 2)" /></span>
      <span><xsl:value-of select="langname" /></span>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>