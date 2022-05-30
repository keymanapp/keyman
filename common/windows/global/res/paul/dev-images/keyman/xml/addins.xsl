<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  
  <xsl:template name="content_addins_style">
    /* Addin Styles */
    #addins {
      position: absolute; 
      left: 0; 
      top: 0; 
      width: 100%; 
      height: expression(
        (document.getElementById('content_addins') ? document.getElementById('content_addins').offsetHeight : 300) - 
        (document.getElementById('addins_control') ? document.getElementById('addins_control').offsetHeight : 100)
      ); 
      overflow-y: auto; 
      padding: 4px
    }
    #addins_control {position: absolute; left: 0; bottom: 0;  padding: 4px 4px 6px 4px; overflow: hidden; border-top: 2px solid #EEEEEE; width: 100%; }
  </xsl:template>  
  
  <xsl:template name="content_addins">
    <div class="header">
      Addins
    </div>
    <div class="content" id="content_addins">
      <div id="addins" class="list">
        <xsl:for-each select="page/KeymanAddinInstalled">
          <xsl:call-template name="addin" />
        </xsl:for-each>
      </div>
      <div id="addins_control">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Install addin</xsl:with-param>
          <xsl:with-param name="command">keyman:addin_install</xsl:with-param>
        </xsl:call-template>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="addin">
    <span class="list_item" tabindex="1">
      <xsl:attribute name="id">list_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="index"><xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:list_mouseover(<xsl:value-of select="index"/>);</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:list_mouseout(<xsl:value-of select="index"/>);</xsl:attribute>
      <div style="float:left">
        <div class="list_expand">
          <xsl:attribute name="onmousedown">javascript:list_detail(<xsl:value-of select="index"/>);</xsl:attribute>
          <xsl:attribute name="id">list_expand_<xsl:value-of select="index"/></xsl:attribute>
          [+]
        </div>
        <xsl:value-of select="name"/>
      </div>
      <div style="float:right">
        <xsl:call-template name="menubutton">
          <xsl:with-param name="caption">Options</xsl:with-param>
          <xsl:with-param name="menutemplate">popupmenu_addinoptions</xsl:with-param>
          <xsl:with-param name="id">options_<xsl:value-of select="index"/></xsl:with-param>
          <xsl:with-param name="align">right</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="popupmenu_addinoptions" />
      </div>
      <div class="list_detail">
        <xsl:attribute name="id">list_detail_<xsl:value-of select="index"/></xsl:attribute>
        <xsl:if test="keyboardname">
          <div>
            <span>Filename:</span>
            <span><xsl:value-of select="filename"/></span>
          </div>
        </xsl:if>
        <xsl:if test="description">
          <div>
            <span>Description:</span>
            <span><xsl:value-of select="description" /></span>
          </div>
        </xsl:if>
        <div>
          <span>Installed for:</span>
          <span><xsl:if test="allusers">All users</xsl:if><xsl:if test="not(allusers)">Current user</xsl:if></span>
        </div>
      </div>
    </span>
  </xsl:template>

  <xsl:template name="popupmenu_addinoptions">
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Uninstall</xsl:with-param>
        <xsl:with-param name="command">keyman:addin_uninstall?index=<xsl:value-of select="index" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Configure...</xsl:with-param>
        <xsl:with-param name="command">keyman:addin_configure?index=<xsl:value-of select="index"/></xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>