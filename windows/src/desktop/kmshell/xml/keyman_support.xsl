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
          <img src="/app/keyman-desktop.png" />
        </div>

        <div class="support_edition">
          <xsl:value-of select="$locale/string[@name='S_Support_Version']"/>&#160;<xsl:value-of select="/Keyman/version-info/@versionWithTag" /><br />
        </div>

        <div class="support_contact">
          <p><xsl:value-of select="$locale/string[@name='S_Support_ContactInstructions_Free']"/></p>
          <xsl:call-template name="button">
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_CommunitySupport']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:link?url=<xsl:value-of select="/Keyman/keyman-com" />/go/<xsl:value-of select="/Keyman/version-info/@versionRelease" />/community</xsl:with-param>
            <xsl:with-param name="width">220px</xsl:with-param>
          </xsl:call-template>
        </div>

        <div class="support_sil">
          <xsl:variable name="originalText" select="$locale/string[@name='S_Support_CreatedBySILGlobal']"/>
          <xsl:variable name="replacement" select="'SIL Global'"/>
          <xsl:variable name="beforePlaceholder" select="substring-before($originalText, '%0:s')"/>
          <xsl:variable name="afterPlaceholder" select="substring-after($originalText, '%0:s')"/>
          <xsl:value-of select="concat($beforePlaceholder, $replacement, $afterPlaceholder)"/>
        </div>

        <div class="support_copyright">
          <xsl:variable name="originalText" select="$locale/string[@name='S_Support_CopyrightSILGlobal']"/>
          <xsl:variable name="replacement" select="'SIL Global'"/>
          <xsl:variable name="beforePlaceholder" select="substring-before($originalText, '%0:s')"/>
          <xsl:variable name="afterPlaceholder" select="substring-after($originalText, '%0:s')"/>
          <xsl:value-of select="concat($beforePlaceholder, $replacement, $afterPlaceholder)"/>
        </div>

        <div class="support_links">
          <h2><xsl:value-of select="$locale/string[@name='S_Support_UsefulLinks']"/></h2>
          <ul>
            <li><a><xsl:attribute name="href">keyman:link?url=<xsl:value-of select="/Keyman/keyman-com"/>/</xsl:attribute>keyman.com</a></li>
            <li><a href="keyman:support_diagnostics"><xsl:value-of select="$locale/string[@name='S_Menu_Diagnostics_Diagnostics']"/></a></li>
            <li><a>
              <xsl:attribute name="href">keyman:link?url=<xsl:value-of select="/Keyman/keyman-com" />/go/<xsl:value-of select="/Keyman/version-info/@versionRelease" />/support</xsl:attribute
              ><xsl:value-of select="$locale/string[@name='S_Button_OnlineSupport']"/></a></li>
          </ul>
        </div>
      </div>
    </div>
  </xsl:template>
</xsl:stylesheet>
