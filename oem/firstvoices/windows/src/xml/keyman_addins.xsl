<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:template name="content_addins_style">
    /* Addin Styles */
    #addins {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: 100%; 
      height: expression(
        (document.getElementById('subcontent_addins') ? document.getElementById('subcontent_addins').offsetHeight : 300) - 
        (document.getElementById('addins_control') ? document.getElementById('addins_control').offsetHeight : 100) - 2
      ); 
      overflow-y: auto; 
      padding: 6px 4px 4px 4px;
    }
    #addins_control {position: absolute; left: 0; bottom: 0;  padding: 4px 4px 6px 4px; overflow: hidden; border-top: 2px solid #EEEEEE; width: 100% }
  </xsl:template>  
  
  <xsl:template name="content_addins">
    <div class="header">
  		<xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Addins']"/>
    </div>
    <div class="content" id="subcontent_addins">
      <div id="addins" class="list">
        <div class="clear"></div>
        <xsl:for-each select="Keyman/KeymanAddinsInstalled/KeymanAddinInstalled">
          <xsl:call-template name="addin" />
        </xsl:for-each>
      </div>
      <div id="addins_control">
        <p><xsl:value-of select="$locale/String[@Id='S_Addins_ToAddOrRemove']"/></p>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="addin">
    <div class="list_item" tabindex="1" tagType="listitem" onmousedown="javascript:this.focus();" style="width:99%">
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onmouseover">return list_hover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>
      <xsl:attribute name="onkeydown">return list_keydown(event,<xsl:value-of select="index"/>);</xsl:attribute> 
      <xsl:attribute name="onblur">return list_unhover(event,'<xsl:value-of select="index"/>');</xsl:attribute>

      <div>
        <xsl:attribute name="class">listitem_title<xsl:if test="not(enabled)"> addin_disabled</xsl:if></xsl:attribute>
        <xsl:attribute name="onmousedown">return list_detail(event,'<xsl:value-of select="index"/>');</xsl:attribute>
        <div class="list_expand" style="float: right; margin-right: 2px;">
          <xsl:attribute name="id">list_expand_<xsl:value-of select="index"/></xsl:attribute>
          <xsl:text></xsl:text>
        </div>

        <div style="float:left; padding: 4px 2px;">
          <xsl:value-of select="description"/>
        </div>

        <br class="clear" />
      </div>
      
			<div class="list_detail">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <div class="addin_menu">
          <div>
            <a>
              <xsl:attribute name="href">keyman:addin_enabledisable?index=<xsl:value-of select="index"/></xsl:attribute>
              <xsl:choose>
                <xsl:when test="enabled"><xsl:value-of select="$locale/String[@Id='S_Addins_Disable']"/></xsl:when>
                <xsl:otherwise><xsl:value-of select="$locale/String[@Id='S_Addins_Enable']"/></xsl:otherwise>
              </xsl:choose>
            </a>
            <xsl:if test="allusers and /Keyman/canelevate">
              <img width="16" height="16">
                <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>shield.png</xsl:attribute>
              </img>
            </xsl:if>
          </div>
          <xsl:if test="enabled">
            <div>
              <a>
                <xsl:attribute name="href">keyman:addin_configure?index=<xsl:value-of select="index"/></xsl:attribute>
                <xsl:value-of select="$locale/String[@Id='S_Addins_Configure']"/>
              </a>
              <xsl:if test="allusers and /Keyman/canelevate">
                <img width="16" height="16">
                  <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>shield.png</xsl:attribute>
                </img>
              </xsl:if>
            </div>
          </xsl:if>
        </div>
        <table style="font-size: 11px;">
          <xsl:if test="filename">
            <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Filename']"/></td>
              <td>
              <span><xsl:value-of select="filename"/></span>
            </td></tr>
          </xsl:if>
          <xsl:if test="description">
            <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_Description']"/></td>
              <td>
              <span><xsl:value-of select="description" /></span>
            </td></tr>
          </xsl:if>
          <tr><td class="table_header"><xsl:value-of select="$locale/String[@Id='S_Caption_InstalledFor']"/></td>
            <td><span><xsl:if test="allusers"><xsl:value-of select="$locale/String[@Id='S_InstalledFor_AllUsers']"/></xsl:if><xsl:if test="not(allusers)"><xsl:value-of select="$locale/String[@Id='S_InstalledFor_CurrentUser']"/></xsl:if></span>
          </td></tr>
        </table>
      </div>

    </div>
  </xsl:template>
  
  </xsl:stylesheet>