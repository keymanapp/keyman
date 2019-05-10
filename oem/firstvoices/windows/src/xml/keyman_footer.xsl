<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
 
  <xsl:template name="footerframe_style"></xsl:template>

  <xsl:template name="footerframe">
    <xsl:if test="not(/Keyman/support/activationstate/activated) and not(/Keyman/activationserver)">
      <div style="float:left">
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_BuyKeymanDesktop']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:footer_buy</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_ActivateKeymanDesktop']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:footer_activate</xsl:with-param>
        </xsl:call-template>
      </div>
    </xsl:if>
    <xsl:if test="/Keyman/support/islight and not(/Keyman/activationserver)">
      <div style="float:left">
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_UpgradeKeymanDesktop']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:footer_buy</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_EnterUpgradeCode']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:footer_activate</xsl:with-param>
        </xsl:call-template>
      </div>
    </xsl:if>
    <div style="float:right; margin-right: 4px">
      <xsl:call-template name="button">
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_OK']"/></xsl:with-param>
				<xsl:with-param name="default">1</xsl:with-param>
        <xsl:with-param name="command">keyman:footer_ok</xsl:with-param>
        <xsl:with-param name="width">70px</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="button">
        <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
        <xsl:with-param name="width">70px</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>

</xsl:stylesheet>