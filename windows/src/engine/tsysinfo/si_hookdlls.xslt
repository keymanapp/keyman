<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:output method="html" encoding="utf-8" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <table>
          <thead>
            <tr>
              <th>Suspicious</th>
              <th>Filename</th>
              <th>A</th>
              <th>W</th>
              <th>C</th>
              <th>U</th>
              <th>Folder</th>
              <th>Company</th>
              <th>Product</th>
              <th>Version</th>
              <th>File Description</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//DLL" />
          </tbody>
        </table>
      </body>
    </html>
  </xsl:template>

<xsl:template match="/KeymanDiagnosticRecord/HookDLLs/DLL">
  <tr class="HookModule">
    <td><xsl:value-of select="Suspicious"/></td>
    <td><xsl:value-of select="FileName"/></td>
    <td><xsl:value-of select="ImportsSetWindowsHookExA"/></td>
    <td><xsl:value-of select="ImportsSetWindowsHookExW"/></td>
    <td><xsl:value-of select="ImportsCallNextHookEx"/></td>
    <td><xsl:value-of select="ImportsUnhookWindowsHookEx"/></td>
    <td><xsl:value-of select="Folder"/></td>
    <td><xsl:value-of select="VersionCompanyName"/></td>
    <td><xsl:value-of select="VersionProductName"/></td>
    <td><xsl:value-of select="VersionProductVersion"/></td>
    <td><xsl:value-of select="VersionFileDescription"/></td>
  </tr>
</xsl:template>

</xsl:stylesheet> 
