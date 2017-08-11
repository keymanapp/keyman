<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <xsl:for-each select="/KeymanDiagnosticRecord/Office/Registry/*/Key">
          <xsl:variable name="Version" select="number(substring(@Path, string-length('HKEY_CURRENT_USER\Software\Microsoft\Office\')+1, 3))" />
          <h1> 
            <xsl:call-template name="OfficeFullName">
              <xsl:with-param name="Version" select="$Version" />
            </xsl:call-template>
          </h1>
          <xsl:call-template name="Registry" />
          <xsl:call-template name="Folder">
            <xsl:with-param name="Folder" select="/KeymanDiagnosticRecord/Office/Files/Program-Files-MicrosoftOffice/*[name(.)=concat('Office',$Version)]"/>
            <xsl:with-param name="Name" select="concat('Program Files\Microsoft Office ',$Version)" />
          </xsl:call-template>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="OfficeFullName">
    <xsl:param name="Version" />
    <xsl:choose>
      <xsl:when test="$Version = 9">Office 2000 (9.0)</xsl:when>
      <xsl:when test="$Version = 10">Office XP (10.0)</xsl:when>
      <xsl:when test="$Version = 11">Office 2003 (11.0)</xsl:when>
      <xsl:when test="$Version = 12">Office 2007 (12.0)</xsl:when>
      <xsl:when test="$Version = 13">Office 2010 (13.0)</xsl:when>
      <xsl:otherwise><xsl:value-of select="$Version"/></xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet> 
