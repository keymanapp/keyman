<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:template name="content_keyboards">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function keyboard_checkclick(n) {
          var k = document.getElementById('keyboardcheck_'+n), li = document.getElementById('list_'+n);
          location.href='keyman:keyboard_clickcheck?index='+n+'&value='+k.checked;
          var liTitle = document.getElementById('listtitle_'+n);
          liTitle.className = k.checked ? 'list_title keyboard_loaded' : 'list_title keyboard_unloaded';
          li.focus();
          return true;
        }
        
        function keyboard_updatecheck(n) {
          var k = document.getElementById('keyboardcheck_'+n), li = document.getElementById('list_'+n);
          if(!k) return false; // for packages
          k.checked = !k.checked;
          location.href='keyman:keyboard_clickcheck?index='+n+'&value='+k.checked;
          var liTitle = document.getElementById('listtitle_'+n);
          liTitle.className = k.checked ? 'list_title keyboard_loaded' : 'list_title keyboard_unloaded';
          li.focus();
          return true;
        }
          
      </script>
    ]]></xsl:text>

    <div class="header" id="keyboards_header">
  		<xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Keyboards']"/>
    </div>
    
    <div class="content" id="subcontent_keyboards">
      <div class="contentBox">
			<xsl:choose>
				<xsl:when test="Keyman/KeymanKeyboardInstalled or Keyman/KeymanPackageInstalled">
					<div id="keyboards" class="list">
            <xsl:for-each select="Keyman/KeymanKeyboardInstalled | Keyman/KeymanPackageInstalled/KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled[count(../KeymanKeyboardInstalled)=1]">
              <xsl:sort select="keyboardname" />
              <xsl:choose>
                <xsl:when test="name(..) = 'KeymanKeyboardsPackageInstalled'">
                  <xsl:call-template name="keyboard">
                    <xsl:with-param name="singlekeyboardpackage">1</xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise><xsl:call-template name="keyboard" /></xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
		        
						<xsl:for-each select="Keyman/KeymanPackageInstalled[count(./KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled)!=1]">
              <xsl:sort select="description" />
		          
								<div class="item">
									<xsl:attribute name="id">package_<xsl:value-of select="index"/></xsl:attribute>
                  <xsl:attribute name="style">left:0;top:0;width:99%;padding:4px 0 2px 0</xsl:attribute>
		              
									<img class="list_icon" style="float: left; margin: 2px 6px 4px 1px;">
      							<xsl:attribute name="src">
      								<xsl:value-of select="/Keyman/templatepath"/>package.gif
      							</xsl:attribute>
									</img>
									<div class="packagetitle">
                    <xsl:value-of select="description"/>
                  </div>
                  <br style="clear:left" />
								</div>
								<xsl:for-each select="KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled">
                  <xsl:sort select="keyboardname" />
									<xsl:call-template name="keyboard" />
								</xsl:for-each>
						</xsl:for-each>
					</div>
				</xsl:when>
				<xsl:otherwise>
					<div id="keyboards" style="position: absolute; top: 25%;">
						<div style="position: relative; left: 0; top: 0; font-size: 13.3px; margin: 0 12px; border: solid 1px gray; padding: 8px; background: #F5F6BE">
							<img style="float: left; margin: 0 8px 0 0">
								<xsl:attribute name="src">
									<xsl:value-of select="/Keyman/templatepath"/>info.png
								</xsl:attribute>
							</img>
							<xsl:value-of select="$locale/String[@Id='S_Keyboards_NoKeyboardsInstalled']"/>
						</div>
					</div>
				</xsl:otherwise>
			</xsl:choose>
      <div id="keyboards_control">
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_InstallKeyboard']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_install</xsl:with-param>
        </xsl:call-template>
      </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="keyboard">
    <xsl:param name="singlekeyboardpackage" />
    <div tabindex="1" tagType="listitem">
      <xsl:attribute name="class">list_item</xsl:attribute>
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>

			<xsl:attribute name="onkeydown">return list_keydown(event,'<xsl:value-of select="index"/>');</xsl:attribute>
			<xsl:attribute name="onmouseover">return list_hover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
			<xsl:attribute name="onmouseout">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onfocus">return list_focus(event,'<xsl:value-of select="index"/>');</xsl:attribute>
			<xsl:attribute name="onblur">return list_blur(event,'<xsl:value-of select="index"/>');</xsl:attribute>
			<xsl:attribute name="oncontextmenu">event.cancelBubble=true;event.returnValue=false;</xsl:attribute>
			<xsl:attribute name="onmousedown">return list_mousedown(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      
      <xsl:attribute name="style">left:0;top:0;width:99%</xsl:attribute>
      <div>
        <xsl:attribute name="id">listtitle_<xsl:value-of select="index"/></xsl:attribute>
        <xsl:attribute name="class">list_title <xsl:choose><xsl:when test="loaded">keyboard_loaded</xsl:when><xsl:otherwise>keyboard_unloaded</xsl:otherwise></xsl:choose></xsl:attribute>
        <xsl:attribute name="onmousedown">return list_detail(event,'<xsl:value-of select="index"/>');</xsl:attribute>
        <div style="float:left; margin-top: 3px;">

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
            <xsl:attribute name="id">keyboard_name_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:if test="keyboardname"><xsl:value-of select="keyboardname"/></xsl:if>
            <xsl:text> </xsl:text>
          </div>
        </div>
        <div style="float: right;">
          <div style="float: left; margin-top: 5px; color: gray;">
            <xsl:attribute name="href">keyman:keyboard_hotkey?index=<xsl:value-of select="index"/></xsl:attribute>
            <xsl:if test="hotkey"><xsl:value-of select="hotkey"/></xsl:if>
            <xsl:if test="not(hotkey)"><xsl:text><!-- work around IE crash --> </xsl:text></xsl:if>
          </div>
          
          <div style="float: left; margin-top: 2px;" tabindex="-1">
            
          <xsl:call-template name="checkbox">
            <xsl:with-param name="id">keyboardcheck_<xsl:value-of select="index"/></xsl:with-param>
            <xsl:with-param name="checked"><xsl:if test="loaded">true</xsl:if></xsl:with-param>
            <xsl:with-param name="title">Enable this keyboard (SPACEBAR)</xsl:with-param>
            <xsl:with-param name="ondblclick">return keyboard_updatecheck('<xsl:value-of select="index"/>');</xsl:with-param>
            <xsl:with-param name="onclick">return keyboard_checkclick('<xsl:value-of select="index"/>');</xsl:with-param>
            <xsl:with-param name="onmousedown">event.cancelBubble = true;event.returnValue = false;return false;</xsl:with-param>
            <xsl:with-param name="tabid">-1</xsl:with-param>
          </xsl:call-template>
          </div>

          <div class="list_expand">
            <xsl:attribute name="id">list_expand_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:text> </xsl:text>
          </div>
        </div>
      </div>
      <div class="list_detail">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <div class="keyboard_options">
          <xsl:if test="ownerpackagename and ../../options">
            <a><xsl:attribute name="href">keyman:keyboard_options?index=<xsl:value-of select="index" /></xsl:attribute><div class="keyboard_options_link"><xsl:value-of select="$locale/String[@Id='S_Menu_Options']"/></div></a>
          </xsl:if>
          <a><xsl:attribute name="href">keyman:keyboard_hotkey?index=<xsl:value-of select="index" /></xsl:attribute><div class="keyboard_options_link">
            <xsl:if test="hotkey"><xsl:value-of select="$locale/String[@Id='S_Menu_ChangeHotkey']"/></xsl:if>
            <xsl:if test="not(hotkey)"><xsl:value-of select="$locale/String[@Id='S_Menu_SetHotkey']"/></xsl:if>
          </div></a>
          <xsl:if test="not(ownerpackagename)">
          <a><xsl:attribute name="href">keyman:keyboard_uninstall?index=<xsl:value-of select="index" /></xsl:attribute><div class="keyboard_options_link">
            <xsl:if test="installedbyadmin and /Keyman/canelevate">
              <img style="border: none; margin-right: 4px; vertical-align: middle" alt=""><xsl:attribute name="src"><xsl:value-of select='/Keyman/templatepath' />shield.png</xsl:attribute></img>
            </xsl:if>
            <xsl:value-of select="$locale/String[@Id='S_Menu_Uninstall']"/>
          </div></a>
          </xsl:if>
          <xsl:if test="ownerpackagename">
          <a><xsl:attribute name="href">keyman:package_welcome?index=<xsl:value-of select="../../index" /></xsl:attribute><div class="keyboard_options_link"><xsl:value-of select="$locale/String[@Id='S_Menu_ShowWelcome']"/></div></a>
          <a><xsl:attribute name="href">keyman:package_uninstall?index=<xsl:value-of select="../../index" /></xsl:attribute><div class="keyboard_options_link">
            <xsl:if test="installedbyadmin and /Keyman/canelevate">
            <img style="border: none; margin-right: 4px; vertical-align: middle" alt=""><xsl:attribute name="src"><xsl:value-of select='/Keyman/templatepath' />shield.png</xsl:attribute></img>
            </xsl:if>
            <xsl:value-of select="$locale/String[@Id='S_Menu_UninstallPackage']"/>
          </div></a>
          </xsl:if>
        </div>
        <div class="keyboard_table">
        <table style="font-size: 11px;">
        <xsl:if test="name">
          <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Filename']"/></td>
            <td>
            <span><xsl:value-of select="name"/>.kmx</span>
          </td></tr>
        </xsl:if>
        <xsl:if test="$singlekeyboardpackage = '1'">
          <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Package']"/></td>
            <td>
              <span><xsl:value-of select="../../description"/></span>
            </td>
          </tr>    
        </xsl:if>
        <xsl:if test="//KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled[name=current()/name]">
          <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Version']"/></td>
            <td>
              <span><xsl:value-of select="../../detail[@name='version']"/></span>
            </td>
          </tr>    
        </xsl:if>
        <xsl:if test="//KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled[name=current()/name]/../../Fonts"><!-- I2216 -->
          <tr>
            <td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Fonts']" /></td>
            <td>
              <xsl:for-each select="//KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled[name=current()/name]/../../Fonts/Font">
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
          
          <tr>
            <td class="table_header">
              <xsl:value-of select="$locale/String[@Id='S_Caption_InstalledFor']"/>
            </td>
            <td>
              <xsl:choose>
                <xsl:when test="installedbyadmin">
                  <xsl:value-of select="$locale/String[@Id='S_InstalledFor_AllUsers']"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="$locale/String[@Id='S_InstalledFor_CurrentUser']"/>
                </xsl:otherwise>
              </xsl:choose>
            </td>
          </tr>
        </table>
        </div>
        <div style="clear:both">&#160;</div>
      </div>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>