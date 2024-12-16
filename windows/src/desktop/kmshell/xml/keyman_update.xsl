<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template name="content_update">

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
          <xsl:value-of select="$locale/string[@name='S_Support_Version']"/>&#160;<xsl:value-of select="/Keyman/version-info/@versionWithTag" /><br />
        </div>

        <div id="update_status">Updates are available which will be applied when Windows is next restarted:</div>

        <div id="update_details">

          <table cellspacing="0" cellpadding="0" id="Updates">
            <tr>
              <th></th>
              <th><xsl:value-of select="$locale/string[@name='S_Update_ComponentHead']"/></th>
              <th><xsl:value-of select="$locale/string[@name='S_Update_OldVersionHead']"/></th>
              <th><xsl:value-of select="$locale/string[@name='S_Update_SizeHead']"/></th>
            </tr>
            <xsl:apply-templates select="/Keyman/Updates/Update" />
          </table>

        </div>

        <div id="update_controls">
          <xsl:call-template name="button">
            <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Update_ApplyNow']"/></xsl:with-param>
            <xsl:with-param name="command">keyman:update_applynow</xsl:with-param>
            <xsl:with-param name="width">220px</xsl:with-param>
          </xsl:call-template>

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
    <tr>
      <td class="UpdateCheckBox">
				<input type="checkbox">
					<xsl:attribute name="onclick">javascript:updateTick("<xsl:value-of select="index" />");</xsl:attribute>
					<xsl:attribute name="id">Update_<xsl:value-of select="index" /></xsl:attribute>
					<xsl:if test="not(@DefaultUnchecked)"><xsl:attribute name="checked">checked</xsl:attribute></xsl:if>
					<xsl:attribute name="name">Update_<xsl:value-of select="index" /></xsl:attribute>
				</input>
        <xsl:if test="RequiresAdmin">
          <span><xsl:attribute name="id">Update_<xsl:value-of select="index" />_RequiresAdmin</xsl:attribute></span>
        </xsl:if>
      </td>
    <xsl:choose>
      <xsl:when test="Package/Text != ''">
        <td class='component'>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Package/Text"/>
          </label>
        </td>
        <td>
          <xsl:value-of select="Package/OldVersion"/>
        </td>
        <td>
          <xsl:value-of select="Package/DownloadSize"/>
        </td>
      </xsl:when>
      <xsl:when test="Keyman/DownloadURL != ''">
        <td>
          <label>
            <xsl:attribute name="for">Update_<xsl:value-of select="index"/></xsl:attribute>
            <xsl:value-of select="Keyman/Text"/>
          </label>
        </td>
        <td>
          <xsl:value-of select="Keyman/OldVersion"/>
        </td>
        <td>
          <xsl:value-of select="Keyman/DownloadSize"/>
        </td>
      </xsl:when>
      <xsl:otherwise>
        <td>
          <div id="DownloadFrom">
            <xsl:value-of select="$locale/string[@name='S_Update_DownloadFrom']"/>
          </div>
          <div id="URL">
            <a href="keyman:openwebsite">
              <xsl:value-of select="/Keyman/DownloadURL"/>
            </a>
          </div>
        </td>
        <td>
          <xsl:value-of select="Keyman/DownloadVersion"/>
        </td>
      </xsl:otherwise>
    </xsl:choose>
    </tr>
  </xsl:template>

</xsl:stylesheet>
