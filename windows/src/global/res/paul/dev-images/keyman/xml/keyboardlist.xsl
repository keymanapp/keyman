<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="content_keyboardlist_style">
    .packagetitle { float: left; font: bold 12pt Tahoma; margin: 2px 0 0 1px; }
    #keyboards {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: 100%; 
      height: expression(
        (document.getElementById('content_keyboards') ? document.getElementById('content_keyboards').offsetHeight : 300) - 
        (document.getElementById('keyboards_control') ? document.getElementById('keyboards_control').offsetHeight : 100)
      ); 
      overflow-y: auto; 
      padding: 6px 4px 4px 4px;
    }
    #keyboards_control {position: absolute; left: 0; bottom: 0; padding: 4px 4px 6px 4px; overflow: hidden; border-top: 2px solid #EEEEEE; width: 100%; }
  </xsl:template>

  <xsl:template name="content_keyboardlist">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function keyboard_checkclick(n)
        {
          var k = document.getElementById('keyboardcheck_'+n);
          location.href='keyman:keyboard_clickcheck?index='+n+'&value='+k.checked;
        }
      </script>
    ]]></xsl:text>

    <div class="header">
      Keyboards
    </div>
    <div class="content" id="content_keyboards">
      <div id="keyboards" class="list">
        <xsl:for-each select="Keyman/KeymanKeyboardInstalled">
          <xsl:call-template name="keyboard" />
        </xsl:for-each>

        <xsl:for-each select="Keyman/KeymanPackageInstalled">
          <div>
            <div class="list_packageitem" tagType='listitem'>
              <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
              <xsl:attribute name="onmouseover">javascript:return list_mouseover(<xsl:value-of select="index"/>);</xsl:attribute>
              <xsl:attribute name="onmouseout">javascript:return list_mouseout(<xsl:value-of select="index"/>);</xsl:attribute>
              <div class="packagetitle"><xsl:value-of select="name"/></div>
              <div style="float: right;" >
                <xsl:call-template name="popupmenu_packageoptions"></xsl:call-template>
                <xsl:call-template name="menubutton">
                  <xsl:with-param name="caption">Package Options</xsl:with-param>
                  <xsl:with-param name="menutemplate">popupmenu_packageoptions</xsl:with-param>
                  <xsl:with-param name="id">options_<xsl:value-of select="index"/></xsl:with-param>
                  <xsl:with-param name="align">right</xsl:with-param>
                  <xsl:with-param name="className">grey</xsl:with-param>
                </xsl:call-template>
              </div>
            </div>
            <xsl:for-each select="KeymanKeyboardsPackageInstalled/KeymanKeyboardInstalled">
              <xsl:call-template name="keyboard" />
            </xsl:for-each>
          </div>
      </xsl:for-each>
      </div>
      <div id="keyboards_control">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Install keyboard</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_install</xsl:with-param>
        </xsl:call-template>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="keyboard">
    <span class="list_item" tabindex="1" tagType='listitem'>
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:return list_mouseover(<xsl:value-of select="index"/>);</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:return list_mouseout(<xsl:value-of select="index"/>);</xsl:attribute>
      <div style="float:left; margin-top: 3px;">
        <div class="list_expand">
          <xsl:attribute name="onmousedown">javascript:list_detail(<xsl:value-of select="index"/>);</xsl:attribute>
          <xsl:attribute name="id">list_expand_<xsl:value-of select="index"/></xsl:attribute>
          <xsl:text></xsl:text>
        </div>
        <div class="list_icon">
          <img style="width: 16px; height:16px;" src="img/no_icon.gif">
            <xsl:if test="bitmap">          
              <xsl:attribute name="src">
                <xsl:value-of select="/Keyman/ImagePath"/>
                <xsl:value-of select="bitmap"/>
              </xsl:attribute>
            </xsl:if>
          </img>
        </div>
        <xsl:text> </xsl:text>
        <xsl:value-of select="keyboardname"/>
      </div>
      <div style="float: right;">
        <div style="float: left; margin-top: 5px; color: gray;">
          <xsl:attribute name="href">keyman:keyboard_hotkey?index=<xsl:value-of select="index"/></xsl:attribute>
          <xsl:if test="hotkey">(<xsl:value-of select="hotkey"/>)</xsl:if>
          <xsl:if test="not(hotkey)">(no hotkey)</xsl:if>
        </div>
        
        <div style="float: left; margin-top: 2px;">
        <xsl:call-template name="checkbox">
          <xsl:with-param name="id">keyboardcheck_<xsl:value-of select="index"/></xsl:with-param>
          <xsl:with-param name="checked"><xsl:if test="loaded">true</xsl:if></xsl:with-param>
          <xsl:with-param name="onclick">javascript:keyboard_checkclick(<xsl:value-of select="index"/>);</xsl:with-param>
        </xsl:call-template>
          </div>

        <xsl:call-template name="menubutton">
          <xsl:with-param name="caption">Options</xsl:with-param>
          <xsl:with-param name="menutemplate">popupmenu_keyboardoptions</xsl:with-param>
          <xsl:with-param name="id">options_<xsl:value-of select="index"/></xsl:with-param>
          <xsl:with-param name="className">grey</xsl:with-param>
          <xsl:with-param name="align">right</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="popupmenu_keyboardoptions" />
      </div>
      <div class="list_detail">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <table style="font: 8pt Tahoma;">
        <xsl:if test="keyboardname">
          <tr><td class="table_header">Filename:</td>
            <td>
            <span><xsl:value-of select="keyboardname"/></span>
          </td></tr>
        </xsl:if>
        <xsl:if test="encodings">
          <tr><td class="table_header">Encodings:</td>
            <td>
              <xsl:for-each select="encodings/encoding">
                <xsl:value-of select="."/>
                <xsl:if test="not(last())">,</xsl:if>
              </xsl:for-each>
          </td></tr>
        </xsl:if>
        <tr><td class="table_header">Visual Keyboard:</td>
          <td>
            <xsl:if test="visualkeyboard">Installed</xsl:if>
            <xsl:if test="not(visualkeyboard)">Not installed</xsl:if>
        </td></tr>
        <xsl:if test="message">
          <tr><td class="table_header">Message:</td>
            <td>
              <xsl:value-of select="message"/>
          </td></tr>
        </xsl:if>
        <xsl:if test="copyright">
          <tr><td class="table_header">Copyright:</td>
            <td>
              <xsl:value-of select="copyright"/>
          </td></tr>
        </xsl:if>
        </table>
      </div>
    </span>
  </xsl:template>

  <xsl:template name="popupmenu_keyboardoptions">
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:if test="not(ownerpackagename)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Uninstall</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_uninstall?index=<xsl:value-of select="index" /></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">
          <xsl:if test="hotkey">Change hotkey</xsl:if>
          <xsl:if test="not(hotkey)">Set hotkey</xsl:if>
        </xsl:with-param>
        <xsl:with-param name="command">keyman:keyboard_hotkey?index=<xsl:value-of select="index"/></xsl:with-param>
      </xsl:call-template>
      <xsl:if test="visualkeyboard">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Uninstall visual keyboard</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_uninstallvisualkeyboard?index=<xsl:value-of select="index"/></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(visualkeyboard)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Install visual keyboard...</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_installvisualkeyboard?index=<xsl:value-of select="index"/></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template name="popupmenu_packageoptions">
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Uninstall</xsl:with-param>
        <xsl:with-param name="command">keyman:package_uninstall?index=<xsl:value-of select="index" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Show Introductory Help</xsl:with-param>
        <xsl:with-param name="command">keyman:package_welcome?index=<xsl:value-of select="index"/></xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>