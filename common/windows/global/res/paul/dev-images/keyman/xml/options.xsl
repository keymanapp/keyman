<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  
  <xsl:template name="content_options_style">
    /* Keyboard Styles */
    .optiontitle { font: 12pt Tahoma }
    #options {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: 100%; 
      height: 100%;
      overflow-y: auto; 
      padding: 4px
    }
    .options_list { }
    .options_list_item {
      font: 10pt Tahoma;
    }
    .options_list_group {
      border-bottom: 1px solid silver;
      margin-bottom: 8px;
      padding-bottom: 4px;
    }
    .options_list_header {
      font: bold 10pt Tahoma;
    }
    .options_list_help {
      display: none;
    }
  </xsl:template>
  <xsl:template name="content_options">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function options_checkclick(n)
        {
          var k = document.getElementById('optionscheck_'+n);
          location.href='keyman:options_clickcheck?index='+n+'&value='+k.checked;
        }
        function options_list_mouseover(n) {
          return true;
        }
        function options_list_mouseout(n) {
          return true;
        }

      </script>
    ]]></xsl:text>
    <div class="header">
      Options
    </div>
    <div class="content">
      <div id="options" class="options_list">
        <xsl:for-each select="//group[not(. = following::group)]">
          <xsl:sort select="group"/>
          <div class="options_list_group">
            <div class="options_list_header"><xsl:value-of select="."/></div>
            <xsl:for-each select="//KeymanOption[group=current()]">
              <xsl:sort select="description"/>
              <xsl:call-template name="option" />
            </xsl:for-each>
          </div>
        </xsl:for-each>            
      </div>
    </div>
  </xsl:template>

  <xsl:template name="option">
    <span class="options_list_item" tabindex="1">
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:options_list_mouseover(<xsl:value-of select="index"/>);</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:options_list_mouseout(<xsl:value-of select="index"/>);</xsl:attribute>
      <div>
        <!--<div class="list_expand">
          <xsl:attribute name="onmousedown">javascript:list_detail(<xsl:value-of select="index"/>);</xsl:attribute>
          <xsl:attribute name="id">list_expand_<xsl:value-of select="index"/></xsl:attribute>
          [+]
        </div>-->
        <div style="vertical-align: middle; display: inline;">
          <xsl:call-template name="checkbox">
            <xsl:with-param name="id">optionscheck_<xsl:value-of select="index"/></xsl:with-param>
            <xsl:with-param name="checked"><xsl:if test="value">true</xsl:if></xsl:with-param>
            <xsl:with-param name="onclick">javascript:options_checkclick(<xsl:value-of select="index"/>);</xsl:with-param>
          </xsl:call-template>
        </div>
        <!--<img>
          <xsl:attribute name="src">
            <xsl:value-of select="/page/ImagePath"/>
            <xsl:value-of select="bitmap"/>
          </xsl:attribute>
        </img>
        <xsl:text xml:space="preserve"> </xsl:text>-->
        <div style="vertical-align: middle; display: inline; margin-left: 3px;">
          <xsl:value-of select="description"/>
        </div>
      </div>

      <div class="options_list_help">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <div>
          <xsl:value-of select="helptext"/>
        </div>
      </div>
    </span>
  </xsl:template>
  
  </xsl:stylesheet>