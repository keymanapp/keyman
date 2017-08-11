<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt">

  <xsl:variable name="AllGuids">
    <xsl:for-each select="/root/file">
      <xsl:apply-templates mode="AllGuids" select="document(.)//*[@Guid]">
        <xsl:with-param name="file"><xsl:value-of select="current()"/></xsl:with-param>
      </xsl:apply-templates>
    </xsl:for-each>  
  </xsl:variable>
  
  <xsl:variable name="SortedGuids">
    <xsl:for-each select="msxsl:node-set($AllGuids)//*">
        <xsl:sort select="@Guid"/>
        <xsl:copy-of select="." />
    </xsl:for-each>
  </xsl:variable>
  
  <xsl:variable name="ErrorGuids">
    <xsl:for-each select="msxsl:node-set($SortedGuids)//*">
      <xsl:if test="@Guid = following::node()[1]/@Guid">
        <Duplicate>
          <xsl:copy-of select="following::node()[1]"/>
          <xsl:copy-of select="."/>
        </Duplicate>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>

  <xsl:template match="/root">
    <Root>
      <Duplicates>
        <xsl:copy-of select="msxsl:node-set($ErrorGuids)" />
      </Duplicates>
      <Guids>
        <xsl:copy-of select="msxsl:node-set($AllGuids)" />
      </Guids>
    </Root>
  </xsl:template>
  
  <xsl:template mode="AllGuids" match="//*[@Guid]">
    <xsl:param name="file" />
    <xsl:element name="{local-name()}">
      <xsl:attribute name="file"><xsl:value-of select="$file"/></xsl:attribute>
      <xsl:attribute name="Id"><xsl:value-of select="@Id"/></xsl:attribute>
      <xsl:attribute name="Guid">
        <xsl:choose>
          <xsl:when test="substring(@Guid,1,1)='{'"><xsl:value-of select="substring(@Guid,2,string-length(@Guid)-2)"/></xsl:when>
          <xsl:otherwise><xsl:value-of select="@Guid"/></xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
