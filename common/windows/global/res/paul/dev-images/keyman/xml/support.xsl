<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="content_support_style">
    #support_content { padding: 5px 10px 5px 10px; overflow: auto; }
    .support_header { font: 18pt Tahoma; margin: 10px 0 5px 0; }
    
    .support_details_header { font: bold 10pt Tahoma; display: inline; padding: 2px 2px 3px 8px; text-align: right; }
    .support_details { font: 10pt Tahoma; display: inline; padding: 2px 16px 3px 4px; border-left: 1px solid #ECF8FE; }
  </xsl:template>
  <xsl:template name="content_support">
    <div class="header">
      Support
    </div>
    <div class="content">
      <div id="support_content">
        <div class="support_header">About Keyman</div>
        <table>
          <tr>
            <td class="support_details_header">Version</td>
            <td class="support_details"><xsl:value-of select="/Keyman/support/version" /></td>
          </tr>
          <tr>
            <td class="support_details_header">Engine Version</td>
            <td class="support_details"><xsl:value-of select="/Keyman/support/engineversion" /></td>
          </tr>
          <tr>
            <td class="support_details_header">Engine Path</td>
            <td class="support_details"><xsl:value-of select="/Keyman/support/engineinstallpath" /></td>
          </tr>      
        </table>
        
        <div class="support_header">Registration Status</div>
        <table>
          <tr>
            <td class="support_details_header">Activated</td>
            <td class="support_details">
              <xsl:if test="/Keyman/support/activationstate/activated">Yes</xsl:if>
              <xsl:if test="not(/Keyman/support/activationstate/activated)">No</xsl:if>
            </td>
          </tr>
          <xsl:if test="not(/Keyman/support/activationstate/activated)">
          <tr>
            <td class="support_details_header">Days Used</td>
            <td class="support_details"><xsl:value-of select="/Keyman/support/activationstate/daysused" /></td>
          </tr>
          </xsl:if>
        </table>

        <p>
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Diagnostics</xsl:with-param>
            <xsl:with-param name="command">keyman:support_diagnostics</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Online Support</xsl:with-param>
            <xsl:with-param name="command">keyman:support_online</xsl:with-param>
          </xsl:call-template>
        </p>
      </div>
    </div>
  </xsl:template>

</xsl:stylesheet>