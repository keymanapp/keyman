<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template name="content_update">
  <xsl:variable name="isNewKeymanVersionAvailable" select="boolean(/Keyman/Updates/Update/Keyman/NewVersion)"/>
  <xsl:variable name="isNewKeyboardVersionAvailable" select="boolean(/Keyman/Updates/Update/Package/NewVersion)"/>

    <div class="header">
      <xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/string[@name='S_Update']"/>
    </div>

    <div id="subcontent_update" class="content">
      <div id="update_content">

        <div class="update_title">
          <img src="/app/keyman-desktop.png" />
        </div>

        <div class="update_edition">
          <xsl:value-of select="$locale/string[@name='S_Support_Version']"/>&#160;<xsl:value-of select="/Keyman/version-info/@versionWithTag" />
        </div>

        <div id="update_status">
            <xsl:if test="$isNewKeymanVersionAvailable or $isNewKeyboardVersionAvailable">
                Updates are available which will be applied when Windows is next restarted:
            </xsl:if>
            <xsl:if test="not($isNewKeymanVersionAvailable or $isNewKeyboardVersionAvailable)">
                No updates are available.
            </xsl:if>
        </div>

        <div class="grid_container_update" id="update_details">
            <div class='grid_item'><xsl:value-of select="$locale/string[@name='S_Update_ComponentHead']"/></div>
            <div class='grid_item'><xsl:value-of select="$locale/string[@name='S_Update_OldVersionHead']"/></div>
            <div class='grid_item'><xsl:value-of select="$locale/string[@name='S_Update_SizeHead']"/></div>

            <xsl:apply-templates select="/Keyman/Updates/Update" />

        </div>

        <div class="update_controls" id="update_controls">

          <xsl:choose>
            <xsl:when test="$isNewKeymanVersionAvailable or $isNewKeyboardVersionAvailable">
              <xsl:call-template name="button">
                <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Update_ApplyNow']"/></xsl:with-param>
                <xsl:with-param name="command">keyman:update_applynow</xsl:with-param>
                <xsl:with-param name="width">220px</xsl:with-param>
              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="button">
                <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Update_ApplyNow']"/></xsl:with-param>
                <xsl:with-param name="command"></xsl:with-param>
                <xsl:with-param name="width">220px</xsl:with-param>
                <xsl:with-param name="disabled">1</xsl:with-param>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>

          <xsl:call-template name="button">
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Update_CheckNow']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:update_checknow</xsl:with-param>
            <xsl:with-param name="width">220px</xsl:with-param>
          </xsl:call-template>
        </div>

        <!-- <li><a href="keyman:support_updatecheck"><xsl:value-of select="$locale/string[@name='S_Button_CheckForUpdates']"/></a></li> -->

      </div>
    </div>
  </xsl:template>

  <xsl:template match="/Keyman/Updates/Update">
    <xsl:choose>
      <xsl:when test="Package/Text != ''">
        <div class='grid_item'>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Package/Text"/>
          </label>
        </div>
        <div class='grid_item'>
          <xsl:value-of select="Package/OldVersion"/>
        </div>
        <div class='grid_item'>
          <xsl:value-of select="Package/DownloadSize"/>
        </div>
      </xsl:when>
      <xsl:when test="Keyman/DownloadURL != ''">
        <div class='grid_item'>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Keyman/Text"/>
          </label>
        </div>
        <div class='grid_item'>
          <xsl:value-of select="Keyman/OldVersion"/>
        </div>
        <div class='grid_item'>
          <xsl:value-of select="Keyman/DownloadSize"/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div class='grid_item'>
          <div id="DownloadFrom">
            <xsl:value-of select="$locale/string[@name='S_Update_DownloadFrom']"/>
          </div>
          <div id="URL">
            <a href="keyman:openwebsite">
              <xsl:value-of select="/Keyman/DownloadURL"/>
            </a>
          </div>
        </div>
        <div class='grid_item'>
          <xsl:value-of select="Keyman/DownloadVersion"/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
