<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:output method="html" encoding="utf-16" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <xsl:apply-templates select="/KeymanDiagnosticRecord/Overview/WindowsVersion" />
        <xsl:apply-templates select="/KeymanDiagnosticRecord/Overview/DiskSpace"/>
        <xsl:apply-templates select="/KeymanDiagnosticRecord/Overview/Memory"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="DiskSpace">
    <h1>Disk Information</h1>
    <table>
      <thead>
        <tr>
          <th>Drive</th>
          <th>Total Size</th>
          <th>Free Space</th>
        </tr>
      </thead>
      <tbody>
        <xsl:apply-templates select="HardDrive" />
      </tbody>
    </table>
  </xsl:template>
                

  <xsl:template match="HardDrive">
    <tr>
      <td><xsl:value-of select="@Drive"/></td>
      <td><xsl:value-of select="@TotalSpace"/></td>
      <td><xsl:value-of select="@TotalFreeSpace"/></td>
    </tr>
  </xsl:template>

  <xsl:template match="Memory">
    <h1>Memory Information</h1>
    <table>
      <thead>
        <tr>
          <th rowspan="2">Memory Load</th>
          <th colspan="2">Physical</th>
          <th colspan="2">Page File</th>
          <th colspan="2">Virtual</th>
        </tr>
        <tr>
          <th>Total</th>
          <th>Free</th>
          <th>Total</th>
          <th>Free</th>
          <th>Total</th>
          <th>Free</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td><xsl:value-of select="MemoryLoad"/></td>
          <td><xsl:value-of select="TotalPhysical"/></td>
          <td><xsl:value-of select="FreePhysical"/></td>
          <td><xsl:value-of select="TotalPageFile"/></td>
          <td><xsl:value-of select="FreePageFile"/></td>
          <td><xsl:value-of select="TotalVirtual"/></td>
          <td><xsl:value-of select="FreeVirtual"/></td>
        </tr>
      </tbody>
    </table>
  </xsl:template>

  <xsl:template match="/KeymanDiagnosticRecord/Overview/WindowsVersion">
    <h1>Windows Version</h1>
    <table>
      <tbody>
        <tr>
          <th>OS Name</th>
          <td>
            <xsl:choose>
              <xsl:when test="contains(., 'Server')">
                <xsl:choose>
                  <xsl:when test="@MajorVersion = '5' and @MinorVersion = '0'">Windows 2000 Server</xsl:when>
                  <xsl:when test="@MajorVersion = '5' and @MinorVersion = '2'">Windows Server 2003</xsl:when>
                  <xsl:when test="@MajorVersion = '6' and @MinorVersion = '0'">Windows Server 2008</xsl:when>
                  <xsl:when test="@MajorVersion = '6' and @MinorVersion = '1'">Windows Server 2008 R2</xsl:when>
                  <xsl:otherwise>
                    Unknown Server (<xsl:value-of select="."/>)
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:when>
              <xsl:otherwise>
                <xsl:choose>
                  <xsl:when test="@MajorVersion = '5' and @MinorVersion = '0'">Windows 2000</xsl:when>
                  <xsl:when test="@MajorVersion = '5' and @MinorVersion = '1'">Windows XP</xsl:when>
                  <xsl:when test="@MajorVersion = '5' and @MinorVersion = '2'">Windows XP x64</xsl:when>
                  <xsl:when test="@MajorVersion = '6' and @MinorVersion = '0'">Windows Vista</xsl:when>
                  <xsl:when test="@MajorVersion = '6' and @MinorVersion = '1'">Windows 7</xsl:when>
                  <xsl:otherwise>
                    Unknown Client (<xsl:value-of select="."/>)
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:otherwise>
            </xsl:choose>
          </td>
        </tr>
        <tr>
          <th>Service Pack</th>
          <td>
            <xsl:value-of select="@CSDVersion"/>
          </td>
        </tr>
        <tr>
          <th>Version Number</th>
          <td>
            <xsl:value-of select="@MajorVersion"/>.<xsl:value-of select="@MinorVersion"/>.<xsl:value-of select="@BuildNumber"/>
          </td>
        </tr>
        <tr>
          <th>x64 Edition?</th>
          <td>
            <xsl:value-of select="@x64"/>
          </td>
        </tr>
      </tbody>
    </table>
  </xsl:template>

</xsl:stylesheet> 
