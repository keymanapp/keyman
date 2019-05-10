<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  
  <xsl:template name="content_options_style">
    /* Keyboard Styles */
    .optiontitle { font-size: 16px; }
    #options {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: expression(document.body.clientWidth - <xsl:value-of select="$locale_keyman/menuwidth"/>); 
      height: expression(
        (document.getElementById('subcontent_options') ? document.getElementById('subcontent_options').offsetHeight : 300) - 
        (100) - 2
      ); 
      overflow-y: auto; 
      padding: 6px 4px 4px 4px;
    }
    .options_list { }
    .options_list_item {
      font-size: 13.3px;
      display: block;
      padding-bottom: 1px;
    }
    .options_list_footer {
      border-bottom: 1px solid silver;
      margin-bottom: 8px;
      padding-bottom: 4px;
    }
    .options_list_header {
      font-size: 13.3px; font-weight: bold;
    }
    .options_list_help {
      display: none;
    }
  </xsl:template>
  <xsl:template name="content_options">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function options_list_mouseover(n) {
          return true;
        }
        function options_list_mouseout(n) {
          return true;
        }
        
        function options_updatecheck(n) {
          var k = document.getElementById('optionscheck_'+n);
          k.checked = !k.checked;
          location.href='keyman:options_clickcheck?index='+n+'&value='+k.checked;
          document.getElementById('list_'+n).focus();
        }
      </script>
    ]]></xsl:text>

		<div class="header">
  		<xsl:call-template name="header_helplinks" />
			<xsl:value-of select="$locale/String[@Id='S_Options']"/>
		</div>

		<div class="content" id="subcontent_options">
      <div id="options" class="options_list">
        <xsl:for-each select="//OptionGroups/OptionGroup">
					<xsl:sort select="sort" />
          <div class="options_list_header"><xsl:value-of select="$locale/String[@Id=current()/name]"/></div>
          <xsl:for-each select="//KeymanOption[group=current()/name]">
            <xsl:if test="name != 'koRunElevatedInVista'">
              <xsl:call-template name="option" />
            </xsl:if>
          </xsl:for-each>
					<xsl:if test="current() != //OptionGroups/OptionGroup[last()]">
						<div class="options_list_footer"></div>
					</xsl:if>
        </xsl:for-each>            
      </div>
    </div>
  </xsl:template>

  <xsl:template name="option">
		<xsl:if test="name='koShowHints'">
			<div style="float:right">
				<xsl:call-template name="button">
					<xsl:with-param name="caption">
						<xsl:value-of select="$locale/String[@Id='S_Button_ResetHints']"/>
					</xsl:with-param>
					<xsl:with-param name="command">keyman:options_resethints</xsl:with-param>
				</xsl:call-template>
			</div>
		</xsl:if>
		<div class="list_item" tagType="listitem" onmousedown="javascript:this.focus();">
      <xsl:if test="enabled">
        <xsl:attribute name="tabindex">1</xsl:attribute>
        <xsl:attribute name="ondblclick">javascript:options_updatecheck('<xsl:value-of select="index"/>');</xsl:attribute>
        <xsl:attribute name="onclick">javascript:options_updatecheck('<xsl:value-of select="index"/>');</xsl:attribute>
      </xsl:if>
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onblur">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <div>
        <div style="vertical-align: middle; display: inline;">
          <xsl:call-template name="checkbox">
            <xsl:with-param name="id">optionscheck_<xsl:value-of select="index"/></xsl:with-param>
            <xsl:with-param name="checked"><xsl:if test="value">true</xsl:if></xsl:with-param>
            <xsl:with-param name="disabled"><xsl:if test="not(enabled)">true</xsl:if></xsl:with-param>
            <xsl:with-param name="onclick">javascript:event.cancelBubble=true;event.returnValue=true;location.href='keyman:options_clickcheck?index=<xsl:value-of select="index"/>&amp;value='+this.checked;</xsl:with-param>
            <xsl:with-param name="tabid">-1</xsl:with-param>
          </xsl:call-template>
        </div>
        <div >
          <xsl:choose>
            <xsl:when test="not(enabled)">
              <xsl:attribute name="style">vertical-align: middle; display: inline; margin-left: 3px; color: #808080</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="style">vertical-align: middle; display: inline; margin-left: 3px;</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
					<xsl:value-of select="$locale/String[@Id=current()/name]"/>
          <!--<xsl:value-of select="//OptionText[name=current()/name]/text"/>-->
        </div>
      </div>

      <div class="options_list_help">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <div>
          <xsl:value-of select="helptext"/>
        </div>
      </div>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>