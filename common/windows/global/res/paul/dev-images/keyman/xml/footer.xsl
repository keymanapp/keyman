<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
 
  <xsl:template name="footerframe_style"></xsl:template>

  <xsl:template name="footerframe">
    <div style="float:left">
      <xsl:call-template name="button">
        <xsl:with-param name="caption">Activate Keyman</xsl:with-param>
        <xsl:with-param name="command">keyman:footer_activate</xsl:with-param>
      </xsl:call-template>
    </div>
    <div style="float:right">
      <xsl:call-template name="button">
        <xsl:with-param name="caption">OK</xsl:with-param>
        <xsl:with-param name="command">keyman:footer_ok</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="button">
        <xsl:with-param name="caption">Cancel</xsl:with-param>
        <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>

</xsl:stylesheet>