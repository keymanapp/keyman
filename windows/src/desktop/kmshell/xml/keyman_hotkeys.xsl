<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">



  <xsl:template name="content_hotkeys_style">
    /* Hotkey Styles */
    #hotkeys {
      position: absolute;
      left: 0;
      top: 0;
      width: 100%;
      font-size: 13.3px;
      xheight: 100%;
      height: expression( document.getElementById('subcontent_hotkeys').offsetHeight );
      overflow-y: auto;
      padding: 6px 4px 4px 4px;
      border-bottom: 2px solid white;
    }

    .hotkey_title
    {
      background: #e0e5ef;
      font-size: 13.3px;
      font-weight: bold;
      color: #001020;
      padding: 4px 8px;
    }
  </xsl:template>

  <xsl:template name="content_hotkeys">
    <div class="header">
  		<xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Hotkeys']"/>
    </div>

    <div class="content" id="subcontent_hotkeys">
        <div class="hotkey_title"><xsl:value-of select="$locale/String[@Id='S_Hotkey_Control_Title']"/></div>
        <xsl:for-each select="/Keyman/Hotkeys/Hotkey">
          <xsl:sort select="translate(Target,'012345678','013845672')"/><!--I2302-->
          <xsl:call-template name="hotkey_control" />
        </xsl:for-each>

        <div class="hotkey_title"><xsl:value-of select="$locale/String[@Id='S_Hotkey_Keyboard_Title']"/></div>
        <xsl:for-each select="//KeymanLanguage">
					<xsl:call-template name="hotkey_language" />
				</xsl:for-each>
    </div>

  </xsl:template>

  <xsl:template name="hotkey_language">
    <div class="list_item" tabindex="1" tagType="listitem">
      <xsl:attribute name="id">list_lang_<xsl:value-of select="position()-1"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'lang_<xsl:value-of select="position()-1"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">document.getElementById('list_lang_<xsl:value-of select="position()-1"/>').focus(); return true;</xsl:attribute>

      <div style="float:left; padding: 1px 0px 1px 3px;">

      <div class="list_icon">
        <img style="width: 16px; height:16px;">
          <xsl:choose>
            <xsl:when test="//KeymanKeyboardInstalled[id=current()/keymankeyboardid]/bitmap">
              <xsl:attribute name="src">/data/keyman/bitmap/<xsl:value-of select="//KeymanKeyboardInstalled[id=current()/keymankeyboardid]/bitmap"/>?tag=<xsl:value-of select="/Keyman/PageTag" /></xsl:attribute>
            </xsl:when>
            <xsl:when test="//KeymanKeyboardInstalled[id=current()/keymankeyboardid]">
              <xsl:attribute name="src">/app/no_icon.png</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="src">/app/windows_keyboard.png</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </img>
      </div>

		  <xsl:value-of select="$locale/String[@Id='S_Hotkey_Language_Prefix']"/> <xsl:value-of select="localename"/> &#x2014; <xsl:value-of select="layoutname"/>
      <xsl:value-of select="$locale/String[@Id='S_Hotkey_Language_Suffix']"/>
      </div>
      <div style="float:right">
        <div style="float: left; padding: 1px 3px 1px 0px;">
          <a class="hotkey" tabindex="-1">
            <xsl:attribute name="href">keyman:hotkey_set?index=hotkey_lang_<xsl:value-of select="position()-1"/></xsl:attribute>
            <xsl:attribute name="onmouseover">this.style.cursor='hand';</xsl:attribute>
            <xsl:choose>
              <xsl:when test="hotkey"><xsl:value-of select="hotkey"/></xsl:when>
              <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_Hotkey_None']"/></xsl:otherwise>
            </xsl:choose>
          </a>
        </div>
      </div>
      <br class="clear" />
    </div>
  </xsl:template>

  <xsl:template name="hotkey_control">
    <div class="list_item" tabindex="1" tagType="listitem">
      <xsl:attribute name="id">list_hotkey_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'hotkey_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">document.getElementById('list_hotkey_<xsl:value-of select="index"/>').focus(); return true;</xsl:attribute>

      <div style="float:left; padding: 1px 0px 1px 3px;">
        <xsl:choose>
          <xsl:when test="Target='0'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_TurnKeymanOff']"/></xsl:when>
          <xsl:when test="Target='1'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_OpenKeyboardMenu']"/></xsl:when>
          <xsl:when test="Target='2'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_ShowOnScreenKeyboard']"/></xsl:when>
          <xsl:when test="Target='3'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_OpenConfiguration']"/></xsl:when>
          <xsl:when test="Target='4'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_ShowKeyboardUsage']"/></xsl:when>
          <xsl:when test="Target='5'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_ShowFontHelper']"/></xsl:when>
          <xsl:when test="Target='6'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_ShowCharacterMap']"/></xsl:when>
          <xsl:when test="Target='7'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_OpenTextEditor']"/></xsl:when>
          <xsl:when test="Target='8'"><xsl:value-of select="$locale/String[@Id='S_Hotkey_SwitchLanguage']"/></xsl:when>
        </xsl:choose>
      </div>
      <div style="float:right">
        <div style="float: left; padding: 1px 3px 1px 0px; color: blue;">
          <a class="hotkey" tabindex="-1">
            <xsl:attribute name="href">keyman:hotkey_set?index=hotkey_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:attribute name="onmouseover">this.style.cursor='hand';</xsl:attribute>
            <xsl:choose>
              <xsl:when test="string-length(Value) > 0"><xsl:value-of select="Value"/></xsl:when>
              <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_Hotkey_None']"/></xsl:otherwise>
            </xsl:choose>
          </a>
        </div>
      </div>
      <br class="clear" />
    </div>
  </xsl:template>

</xsl:stylesheet>