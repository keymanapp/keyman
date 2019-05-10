<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template name="content_support">
    <div class="header">
      <xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/String[@Id='S_Support']"/>
    </div>
        
    <div id="subcontent_support" class="content">
      <div id="support_content">
        <div class="support_title">Keyman for FirstVoices 9.0</div>
        
        <div class="support_logo">
          <img>
            <xsl:attribute name="src">
              <xsl:value-of select="/Keyman/templatepath"/>config-keyman-logo.png
            </xsl:attribute>
          </img>
        </div>
        <div id='support_details'>
          <div class="support_details">
            <div class="support_line_header">
              <xsl:value-of select="$locale/String[@Id='S_Support_Edition']"/><br />
              <xsl:value-of select="$locale/String[@Id='S_Support_Version']"/><br />
              <xsl:value-of select="$locale/String[@Id='S_Support_EngineVersion']"/><br />
            </div>
            <div class="support_line_content">
              <xsl:value-of select="/Keyman/support/productname" /><xsl:value-of select="$locale/String[@Id='S_BETA']"/><br />
              <xsl:value-of select="/Keyman/support/version" /><xsl:value-of select="$locale/String[@Id='S_BETA']"/><br />
              <xsl:value-of select="/Keyman/support/engineversion" /><xsl:value-of select="$locale/String[@Id='S_BETA']"/><br />
            </div>
          </div>
          <br /><br />
          <div class="support_links">
            <div class="support_content_header"><xsl:value-of select="$locale/String[@Id='S_Support_UsefulLinks']"/></div>
            <a href="keyman:link?url=http://www.firstvoices.com/">www.firstvoices.com</a><br />
            <a href="keyman:link?url=http://www.keyman.com/">www.keyman.com</a><br />
            <a href="keyman:link?url=http://help.keyman.com/"><xsl:value-of select="$locale/String[@Id='S_Button_OnlineSupport']"/></a><br />
            <a href="keyman:support_updatecheck"><xsl:value-of select="$locale/String[@Id='S_Button_CheckForUpdates']"/></a><br />          
          </div>
          <div style="clear: both; margin: 20px 0 0 100px;">
            <xsl:call-template name="popupmenu_diagnostics"></xsl:call-template>
              
            <xsl:call-template name="menubutton">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Diagnostics']"/></xsl:with-param>
              <xsl:with-param name="menutemplate">popupmenu_diagnostics</xsl:with-param>
              <xsl:with-param name="id">diagnostics_<xsl:value-of select="index"/></xsl:with-param>
            </xsl:call-template>

            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_ProxyConfig']"/></xsl:with-param>
              <xsl:with-param name="command">keyman:support_proxyconfig</xsl:with-param>
            </xsl:call-template>
          </div>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="popupmenu_diagnostics">
    <div class="menu" style="width: 220px">
      <xsl:attribute name="id">menu_diagnostics_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:attribute name="name">menu_diagnostics_<xsl:value-of select="index"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="id">menu_diagnostics_<xsl:value-of select="index"/>_1</xsl:with-param>
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Menu_Diagnostics_Diagnostics']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:support_diagnostics</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="id">menu_diagnostics_<xsl:value-of select="index"/>_2</xsl:with-param>
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Menu_Diagnostics_CheckLanguages']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:support_checklanguages</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
	
</xsl:stylesheet>