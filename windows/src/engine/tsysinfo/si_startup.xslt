<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:output method="html" encoding="utf-16" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <h1>Startup Processes</h1>
        <table>
          <thead>
            <tr>
              <th>Name</th>
              <th>Filename</th>
              <th>Company</th>
              <th>Product</th>
              <th>Version</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="/KeymanDiagnosticRecord/Startup/Location" />
          </tbody>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="/KeymanDiagnosticRecord/Startup/Location">
    <xsl:if test="count(*) &gt; 1">
      <tr>
        <td colspan="6" class="Location">
          <xsl:value-of select="Location"/>
        </td>
      </tr>
      <xsl:apply-templates select="*" />
    </xsl:if>
  </xsl:template>

  <xsl:template match="/KeymanDiagnosticRecord/Startup/Location/*">
    <xsl:if test="name(.) != 'Location'">
      <tr>
        <td><xsl:value-of select="Name"/></td>
        <td><xsl:value-of select="FileName"/></td>
        <td><xsl:value-of select="VersionCompanyName"/></td>
        <td><xsl:value-of select="VersionProductName"/></td>
        <td><xsl:value-of select="VerisonProductVersion"/></td>
        <td><xsl:value-of select="VersionFileDescription"/></td>
      </tr>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet> 
