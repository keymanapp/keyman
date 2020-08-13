<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template name="content_options">

		<div class="header">
  		<xsl:call-template name="header_helplinks" />
			<xsl:value-of select="$locale/string[@name='S_Options']"/>
		</div>

		<div class="content" id="subcontent_options">
      <div id="options" class="options_list">
        <xsl:for-each select="//OptionGroups/OptionGroup">
					<xsl:sort select="sort" />
          <div class="options_list_header"><xsl:value-of select="$locale/string[@name=current()/name]"/></div>
          <xsl:for-each select="//KeymanOption[group=current()/name]">
            <xsl:if test="optiontype = 1">
              <xsl:call-template name="option" />
            </xsl:if>
          </xsl:for-each>
          <xsl:if test="current() != //OptionGroups/OptionGroup[last()]">
            <div class="options_list_footer"></div>
          </xsl:if>
        </xsl:for-each>
        <div class="options_list_footer"></div>

        <div id='options_control'>
          <xsl:call-template name="button">
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_ProxyConfig']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:support_proxyconfig</xsl:with-param>
          </xsl:call-template>
      
          <xsl:call-template name="button">
            <xsl:with-param name="shield">1</xsl:with-param>
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_BaseKeyboard']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:options_basekeyboard</xsl:with-param>
          </xsl:call-template>
          <span id='options_base_keyboard_current'>(<xsl:value-of select= "//basekeyboard" />)</span>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="option">
		<xsl:if test="id='koShowHints'">
			<div style="float:right; margin-top:-3px">
				<xsl:call-template name="button">
					<xsl:with-param name="caption">
						<xsl:value-of select="$locale/string[@name='S_Button_ResetHints']"/>
					</xsl:with-param>
					<xsl:with-param name="command">keyman:options_resethints</xsl:with-param>
				</xsl:call-template>
			</div>
		</xsl:if>
		<div class="list_item" tagType="listitem" onmousedown="javascript:this.focus();">
      <xsl:if test="enabled">
        <xsl:attribute name="tabindex">1</xsl:attribute>
        <xsl:attribute name="ondblclick">javascript:options_updatecheck('<xsl:value-of select="id"/>');</xsl:attribute>
        <xsl:attribute name="onclick">javascript:options_updatecheck('<xsl:value-of select="id"/>');</xsl:attribute>
      </xsl:if>
      <xsl:attribute name="id">list_option_<xsl:value-of select="id"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'option_<xsl:value-of select="id"/>');</xsl:attribute>
      <div>
        <div style="vertical-align: middle; display: inline;">
          <xsl:call-template name="checkbox">
            <xsl:with-param name="id">optionscheck_<xsl:value-of select="id"/></xsl:with-param>
            <xsl:with-param name="checked"><xsl:if test="value='True'">true</xsl:if></xsl:with-param>
            <xsl:with-param name="disabled"><xsl:if test="not(enabled)">true</xsl:if></xsl:with-param>
            <xsl:with-param name="onclick">javascript:event.cancelBubble=true;event.returnValue=true;location.href='keyman:options_clickcheck?id=<xsl:value-of select="id"/>&amp;value='+this.checked;</xsl:with-param>
            <xsl:with-param name="tabid">-1</xsl:with-param>
          </xsl:call-template>
        </div>
        <div >
          <xsl:choose>
            <xsl:when test="not(enabled)">
              <xsl:attribute name="style">display: inline; margin-left: 3px; color: #808080</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="style">display: inline; margin-left: 3px; </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
			  <xsl:value-of select="$locale/string[@name=current()/id]"/>
        </div>
      </div>

      <div class="options_list_help">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="id"/></xsl:attribute>
        <div>
          <xsl:value-of select="helptext"/>
        </div>
      </div>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>