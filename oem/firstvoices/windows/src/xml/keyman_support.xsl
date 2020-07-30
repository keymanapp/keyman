<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  
  <xsl:template name="content_support">
    <div class="header">
      <xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/string[@name='S_Support']"/>
    </div>
        
    <div id="subcontent_support" class="content">
      <div id="support_content">
        <div class="support_title">
          <img>
            <xsl:attribute name="src">
              <xsl:value-of select="/Keyman/templatepath"/>keyman-desktop.png
            </xsl:attribute>
          </img>
        </div>
        
        <div class="support_edition">
          <xsl:value-of select="$locale/string[@name='S_Support_Version']"/>&#160;<xsl:value-of select="/Keyman/support/version" /><br />
        </div>
        
        <div class="support_contact">
          <p><xsl:value-of select="$locale/string[@name='S_Support_ContactInstructions_Free']"/></p>
          <xsl:call-template name="button">
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_CommunitySupport']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:link?url=https://firstvoices.atlassian.net/servicedesk/customer/portals</xsl:with-param>
            <xsl:with-param name="width">220px</xsl:with-param>
          </xsl:call-template>
        </div>

        <div class="support_firstvoices">
        </div>

        <div class="support_sil">
          <xsl:value-of select="$locale/string[@name='S_Support_CreatedBySIL']"/>
        </div>
        
        <div class="support_copyright">
          <xsl:value-of select="$locale/string[@name='S_Support_Copyright']"/>
        </div>

        <div class="support_links">
          <h2><xsl:value-of select="$locale/string[@name='S_Support_UsefulLinks']"/></h2>
          <ul>
            <li><a href="https://www.firstvoices.com/">www.firstvoices.com</a></li>
            <li><a href="https://www.keyman.com/">www.keyman.com</a></li>
            <li><a href="keyman:support_diagnostics"><xsl:value-of select="$locale/string[@name='S_Menu_Diagnostics_Diagnostics']"/></a></li>
            <li><a href="keyman:support_updatecheck"><xsl:value-of select="$locale/string[@name='S_Button_CheckForUpdates']"/></a></li>
            <li><a href="keyman:link?url=https://keyman.com/go/desktop/10.0/support"><xsl:value-of select="$locale/string[@name='S_Button_OnlineSupport']"/></a></li>
          </ul>
        </div>
      </div>
    </div>
  </xsl:template>
	
</xsl:stylesheet>