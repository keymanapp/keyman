<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output version="1.0" encoding="UTF-8" indent="yes" />
  <xsl:preserve-space elements="*" />

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="string">
    <String>
      <!-- We don't need these attributes in the 'compiled' locale.xml files, only in the source
      <xsl:attribute name="Group"><xsl:value-of select="substring-before(@comment, '/')"/></xsl:attribute>
      <xsl:attribute name="Type"><xsl:value-of select="substring-before(substring-after(@comment, '/'), ': ')"/></xsl:attribute>
      -->
      <xsl:attribute name="Id"><xsl:value-of select="@name" /></xsl:attribute>
      <!--We don't need this attribute in the 'compiled' locale.xml files, only in the source
      <xsl:attribute name="Description"><xsl:value-of select="substring-after(@comment, ': ')"/></xsl:attribute>
      -->
      <xsl:value-of select="." />
    </String>
  </xsl:template>

  <xsl:template match="comment()">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="/resources">
    <Locale>
      <xsl:apply-templates />
    </Locale>
  </xsl:template>

</xsl:stylesheet>