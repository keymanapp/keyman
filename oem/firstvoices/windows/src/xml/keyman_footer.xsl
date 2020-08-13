<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
 
  <xsl:template name="footerframe_style"></xsl:template>

  <xsl:template name="footerframe">
    <div id='footer-top'></div>
    <div style="float:right; padding: 2px 12px">
      <xsl:call-template name="button">
        <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_OK']"/></xsl:with-param>
				<xsl:with-param name="default">1</xsl:with-param>
        <xsl:with-param name="command">keyman:footer_ok</xsl:with-param>
        <xsl:with-param name="width">70px</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="button">
        <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_Cancel']"/></xsl:with-param>
        <xsl:with-param name="command">keyman:footer_cancel</xsl:with-param>
        <xsl:with-param name="width">70px</xsl:with-param>
      </xsl:call-template>
    </div>
      <div id="keyboards_control" style="float: left; display: none;">
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_InstallKeyboard']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_install</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption"><xsl:value-of select="$locale/string[@name='S_Button_DownloadKeyboard']"/></xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_download</xsl:with-param>
        </xsl:call-template>
      </div>
  </xsl:template>

</xsl:stylesheet>