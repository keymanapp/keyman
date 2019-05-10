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
          <xsl:if test="not(/Keyman/support/islight) or (Target != 8 and Target != 6)"><!--I2342-->
            <xsl:call-template name="hotkey_control" />
          </xsl:if>
        </xsl:for-each>
        <div class="hotkey_title"><xsl:value-of select="$locale/String[@Id='S_Hotkey_Keyboard_Title']"/></div>
        <xsl:for-each select="//KeymanKeyboardInstalled[loaded]">
          <xsl:call-template name="hotkey_keyboard" />
        </xsl:for-each>
        <xsl:if test="not(/Keyman/support/islight)">
          <div class="hotkey_title"><xsl:value-of select="$locale/String[@Id='S_Hotkey_Language_Title']"/></div>
          <xsl:for-each select="//KeymanLanguage">
						<xsl:call-template name="hotkey_language" />
					</xsl:for-each>
				</xsl:if>
    </div>
    
  </xsl:template>
  
  <xsl:template name="hotkey_keyboard">
    <div class="list_item" tabindex="1" tagType="listitem">
      <xsl:attribute name="id">list_kbd_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'kbd_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">document.getElementById('list_kbd_<xsl:value-of select="index"/>').focus(); return true;</xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event,'kbd_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event,'kbd_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onblur">return list_unhover(event,'kbd_<xsl:value-of select="index"/>');</xsl:attribute>
      
      <div style="float:left; padding: 1px 0px 1px 3px;">
		  <xsl:value-of select="$locale/String[@Id='S_Hotkey_Keyboard_Prefix']"/> <xsl:value-of select="keyboardname"/> <xsl:value-of select="$locale/String[@Id='S_Hotkey_Keyboard_Suffix']"/>
      </div>
      <div style="float:right">
        <div style="float: left; padding: 1px 3px 1px 0px;">
          <a class="hotkey" tabindex="-1">
            <xsl:attribute name="href">keyman:hotkey_set?index=hotkey_kbd_<xsl:value-of select="index"/></xsl:attribute>
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

  <xsl:template name="hotkey_language">
    <div class="list_item" tabindex="1" tagType="listitem">
      <xsl:attribute name="id">list_lang_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'lang_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">document.getElementById('list_lang_<xsl:value-of select="index"/>').focus(); return true;</xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event,'lang_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event,'lang_<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onblur">return list_unhover(event,'lang_<xsl:value-of select="index"/>');</xsl:attribute>
      
      <div style="float:left; padding: 1px 0px 1px 3px;">
		  <xsl:value-of select="$locale/String[@Id='S_Hotkey_Language_Prefix']"/> <xsl:value-of select="localename"/> (<xsl:value-of select="layoutname"/>) <xsl:value-of select="$locale/String[@Id='S_Hotkey_Language_Suffix']"/>
      </div>
      <div style="float:right">
        <div style="float: left; padding: 1px 3px 1px 0px;">
          <a class="hotkey" tabindex="-1">
            <xsl:attribute name="href">keyman:hotkey_set?index=hotkey_lang_<xsl:value-of select="index"/></xsl:attribute>
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
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">document.getElementById('list_<xsl:value-of select="index"/>').focus(); return true;</xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onblur">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>

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