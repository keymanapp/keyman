<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  

  <xsl:template name="content_languages_style">
    /* Language Styles */
    #languages {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: expression(document.body.clientWidth - <xsl:value-of select="$locale_keyman/menuwidth"/> - 2); 
      height: expression(
        (document.getElementById('subcontent_languages') ? document.getElementById('subcontent_languages').offsetHeight : 300) - 2
      ); 
      overflow-x: hidden;
      overflow-y: auto; 
      padding: 6px 4px 4px 4px;
    } 
    th {
      padding: 0 10 0 0;
    }
  </xsl:template>  

  <xsl:template name="content_languages">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function languageselectkeyboard(n)
        {
          var l = document.getElementById('language_'+n);
          //alert(l.value);
          location.href='keyman:language_keyboardchange?index='+n+'&amp;value='+l.value;
        }
      </script>
    ]]></xsl:text>
    <div class="header">
      <xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Languages']"/>
    </div>
    
    
    <div class="content" id="subcontent_languages">
      <div id="languages">
        <div style="padding: 2px; margin: 4px;">
          <span class="list_header" tabindex="0" style="float: left; width: 160px;">
            <xsl:value-of select="$locale/String[@Id='S_Languages_InstalledLanguage']"/>
          </span>
          <span class="list_header" tabindex="0" style="float: left;">
            <xsl:value-of select="$locale/String[@Id='S_Languages_WindowsLayout']"/>
          </span>
          <span class="list_header" tabindex="0" style="float: right; width: 160px;">
            <xsl:value-of select="$locale/String[@Id='S_Languages_KeymanKeyboard']"/>
          </span>
        </div>
        <xsl:for-each select="Keyman/KeymanLanguages/KeymanLanguage">
          <xsl:sort select="localename" data-type="text" />
          <xsl:sort select="layoutname" data-type="text" />
          <xsl:call-template name="language" />
        </xsl:for-each>
      </div>
    </div>
  </xsl:template>
  
  <xsl:template name="language">
    <div class="list_item" style="background: transparent; clear: both;" tabindex="1" tagType="listitem">
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event, '<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmousedown">if(event.srcElement.tagName != 'SELECT' &amp;&amp; event.srcElement.tagName != 'OPTION') this.focus();</xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event, '<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event, '<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onblur">return list_blur(event, '<xsl:value-of select="index"/>');</xsl:attribute>
      
      <div style="float: right; width: 160px; text-align: right;">
        <select tabindex="1" style="width: 160px;">
          <xsl:attribute name="onchange">javascript:languageselectkeyboard('<xsl:value-of select="index"/>'); event.returnValue=false;</xsl:attribute>
          <xsl:attribute name="id">language_<xsl:value-of select="index"/></xsl:attribute>
          <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
          <xsl:attribute name="onkeydown">javascript:menuframe_keydown();</xsl:attribute>
          <xsl:if test="isime or iskeymanlayout">
            <xsl:attribute name="enabled">false</xsl:attribute>  
          </xsl:if>
          <option value="*"><xsl:value-of select="$locale/String[@Id='S_Languages_UseWindowsLayout']"/></option>
          <xsl:apply-templates mode="language_keyboards" select="//KeymanKeyboardInstalled">
            <xsl:with-param name="selectedkeyboardname"><xsl:value-of select="keymankeyboardname"/></xsl:with-param>
          </xsl:apply-templates>
        </select>
      </div>

      <div style="float: left; width: 160px; text-align: left; padding: 4px 0px 0px 3px;">
        <xsl:value-of select="localename"/>
      </div>

      <div style="padding: 4px 0px 0px 3px;">
        <xsl:value-of select="layoutname"/>
      </div>
      <div style="clear:left;height:6px;font-size:1px">&#160;</div>
    </div>
  </xsl:template>
  
  <xsl:template mode="language_keyboards" match="//KeymanKeyboardInstalled">
    <xsl:param name="selectedkeyboardname" />
    <option>
      <xsl:attribute name="value"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:if test="$selectedkeyboardname = name"><xsl:attribute name="selected" /></xsl:if>
      <xsl:value-of select="keyboardname" />
    </option>
  </xsl:template>

</xsl:stylesheet>