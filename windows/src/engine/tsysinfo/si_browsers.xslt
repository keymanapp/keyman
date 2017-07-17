<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:output method="html" encoding="utf-16" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <h1>Browser Information</h1>
        <table>
          <tbody>
            <tr>
              <th>Internet Explorer Version</th>
              <td>
                <xsl:value-of select="/KeymanDiagnosticRecord/Browsers/InternetExplorer/@Version"/>
              </td>
            </tr>
          </tbody>
        </table>
        <h1>Internet Explorer Script Information</h1>
        <table>
          <thead>
            <tr>
              <th>ID</th>
              <th>Script Name</th>
              <th>Proportional Font</th>
              <th>Fixed Font</th>
              <th>Default</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="/KeymanDiagnosticRecord/Browsers/InternetExplorer/Registry/CurrentUserScripts/Key/Key">
              <xsl:sort select="number(substring(@Path,string-length('HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\International\Scripts\')+1))" data-type="number"/>
            </xsl:apply-templates>
          </tbody>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="Key">
    <xsl:variable name="ID" select="number(substring(@Path,string-length('HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\International\Scripts\')+1))"/>
    <tr>
      <td>
        <xsl:value-of select="$ID" />
      </td>
      <td>
        <xsl:call-template name="Script">
          <xsl:with-param name="ID" select="$ID"/>
        </xsl:call-template>
      </td>
      <td>
        <xsl:value-of select="Value[@Name='IEPropFontName']/@Value"/>
      </td>
      <td>
        <xsl:value-of select="Value[@Name='IEFixedFontName']/@Value"/>
      </td>
      <td>
        <xsl:if test="number(../Value[@Name='Default_Script']/@Value)=$ID">Default</xsl:if>
      </td>
    </tr>
  </xsl:template>

  <xsl:template name="Script">
    <xsl:param name="ID" />
    <xsl:variable name="NID" select="number($ID)"/>
    <xsl:choose>
      <xsl:when test="$NID = 0">Default</xsl:when>
      <xsl:when test="$NID = 1">Merge</xsl:when>
      <xsl:when test="$NID = 2">Asciisym</xsl:when>
      <xsl:when test="$NID = 3">Latin based</xsl:when>
      <xsl:when test="$NID = 4">Latin</xsl:when>
      <xsl:when test="$NID = 5">Greek</xsl:when>
      <xsl:when test="$NID = 6">Cyrillic</xsl:when>
      <xsl:when test="$NID = 7">Armenian</xsl:when>
      <xsl:when test="$NID = 8">Hebrew</xsl:when>
      <xsl:when test="$NID = 9">Arabic</xsl:when>
      <xsl:when test="$NID = 10">Devanagari</xsl:when>
      <xsl:when test="$NID = 11">Bengali</xsl:when>
      <xsl:when test="$NID = 12">Gurmukhi</xsl:when>
      <xsl:when test="$NID = 13">Gujarati</xsl:when>
      <xsl:when test="$NID = 14">Oriya</xsl:when>
      <xsl:when test="$NID = 15">Tamil</xsl:when>
      <xsl:when test="$NID = 16">Telugu</xsl:when>
      <xsl:when test="$NID = 17">Kannada</xsl:when>
      <xsl:when test="$NID = 18">Malayalam</xsl:when>
      <xsl:when test="$NID = 19">Thai</xsl:when>
      <xsl:when test="$NID = 20">Lao</xsl:when>
      <xsl:when test="$NID = 21">Tibetan</xsl:when>
      <xsl:when test="$NID = 22">Georgian</xsl:when>
      <xsl:when test="$NID = 23">Korean</xsl:when>
      <xsl:when test="$NID = 24">Japanese</xsl:when>
      <xsl:when test="$NID = 25">Chinese Traditional</xsl:when>
      <xsl:when test="$NID = 26">Chinese Simplified</xsl:when>
      <xsl:when test="$NID = 27">Ethiopic</xsl:when>
      <xsl:when test="$NID = 28">Canadian Syllabic</xsl:when>
      <xsl:when test="$NID = 29">Cherokee</xsl:when>
      <xsl:when test="$NID = 30">Yi</xsl:when>
      <xsl:when test="$NID = 31">Braille</xsl:when>
      <xsl:when test="$NID = 32">Runic</xsl:when>
      <xsl:when test="$NID = 33">Ogham</xsl:when>
      <xsl:when test="$NID = 34">Sinhala</xsl:when>
      <xsl:when test="$NID = 35">Syriac</xsl:when>
      <xsl:when test="$NID = 36">Myanmar</xsl:when>
      <xsl:when test="$NID = 37">Khmer</xsl:when>
      <xsl:when test="$NID = 38">Thaana</xsl:when>
      <xsl:when test="$NID = 39">Mongolian</xsl:when>
      <xsl:when test="$NID = 40">Default</xsl:when>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet> 
