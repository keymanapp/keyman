<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:template match="String">
    <string>
      <xsl:attribute name="name"><xsl:value-of select="@Id" /></xsl:attribute>
      <xsl:attribute name="comment"><xsl:value-of select="@Group" />/<xsl:value-of select="@Type" />: <xsl:value-of select="@Description" 
        /><xsl:if test="@Parameters"> [<xsl:value-of select="@Parameters" />]</xsl:if
        ><xsl:if test="@Version"> (introduced <xsl:value-of select="@Version" />)</xsl:if></xsl:attribute>
      <xsl:value-of select="." />
    </string>
  </xsl:template>

  <xsl:template match="comment()">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="/Locale">
    <resources>
      <xsl:apply-templates select="String|comment()" />
    </resources>
  </xsl:template>

</xsl:stylesheet>