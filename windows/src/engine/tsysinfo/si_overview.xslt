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
        <tr>
          <th>Native Machine</th>
          <td>
            <xsl:choose>
              <xsl:when test="@NativeMachine = '014C'">Intel 386</xsl:when>
              <xsl:when test="@NativeMachine = '0162'">MIPS R3000</xsl:when>
              <xsl:when test="@NativeMachine = '0160'">MIPS R3000</xsl:when>
              <xsl:when test="@NativeMachine = '0166'">MIPS R4000</xsl:when>
              <xsl:when test="@NativeMachine = '0168'">MIPS R10000</xsl:when>
              <xsl:when test="@NativeMachine = '0169'">MIPS WCE v2</xsl:when>
              <xsl:when test="@NativeMachine = '0184'">Alpha AXP</xsl:when>
              <xsl:when test="@NativeMachine = '01A2'">SH3</xsl:when>
              <xsl:when test="@NativeMachine = '01A3'">SH3 DSP</xsl:when>
              <xsl:when test="@NativeMachine = '01A4'">SH3E</xsl:when>
              <xsl:when test="@NativeMachine = '01A6'">SH4</xsl:when>
              <xsl:when test="@NativeMachine = '01A8'">SH5</xsl:when>
              <xsl:when test="@NativeMachine = '01C0'">ARM</xsl:when>
              <xsl:when test="@NativeMachine = '01C2'">ARM Thumb</xsl:when>
              <xsl:when test="@NativeMachine = '01C4'">ARM Thumb 2</xsl:when>
              <xsl:when test="@NativeMachine = '01D3'">TAM33BD</xsl:when>
              <xsl:when test="@NativeMachine = '01F0'">PowerPC</xsl:when>
              <xsl:when test="@NativeMachine = '01F1'">PowerPC FP</xsl:when>
              <xsl:when test="@NativeMachine = '0200'">Intel IA64</xsl:when>
              <xsl:when test="@NativeMachine = '0266'">MIPS 16</xsl:when>
              <xsl:when test="@NativeMachine = '0284'">Alpha 64</xsl:when>
              <xsl:when test="@NativeMachine = '0366'">MIPSFPU</xsl:when>
              <xsl:when test="@NativeMachine = '0466'">AXP64</xsl:when>
              <xsl:when test="@NativeMachine = '0520'">Infineon</xsl:when>
              <xsl:when test="@NativeMachine = '0CEF'">CEF</xsl:when>
              <xsl:when test="@NativeMachine = '0EBC'">EBC (EFI Byte Code)</xsl:when>
              <xsl:when test="@NativeMachine = '8664'">AMD64</xsl:when>
              <xsl:when test="@NativeMachine = '9041'">M32R</xsl:when>
              <xsl:when test="@NativeMachine = 'AA64'">ARM64</xsl:when>
              <xsl:when test="@NativeMachine = 'C0EE'">CEE</xsl:when>
              <xsl:otherwise><xsl:value-of select="@NativeMachine" /></xsl:otherwise>
            </xsl:choose>
          </td>
        </tr>
      </tbody>
    </table>
  </xsl:template>

</xsl:stylesheet>
