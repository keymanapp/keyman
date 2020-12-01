<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <h1>Keyman Desktop 8</h1>

        <xsl:call-template name="KeymanInstalledKeyboards">
          <xsl:with-param name="Version" select="'8.0'" />
        </xsl:call-template>

        <xsl:call-template name="KeymanActiveKeyboards">
          <xsl:with-param name="Version" select="'8.0'" />
        </xsl:call-template>

        <xsl:call-template name="KeymanActiveLanguages">
          <xsl:with-param name="Version" select="'8.0'" />
        </xsl:call-template>

        <h1>Keyman Desktop 9</h1>

        <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/LanguageProfiles">
          <xsl:with-param name="Enabled" select="'True'" />
        </xsl:apply-templates>
        <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/LanguageProfiles" />

        <xsl:call-template name="KeymanInstalledKeyboards">
          <xsl:with-param name="Version" select="'9.0'" />
        </xsl:call-template>

        <xsl:call-template name="KeymanActiveKeyboards">
          <xsl:with-param name="Version" select="'9.0'" />
        </xsl:call-template>

        <h1>Registry</h1>

        <h2>Local Machine registry settings</h2>

        <xsl:for-each select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/LocalMachine/Key">
          <xsl:call-template name="Registry" />
        </xsl:for-each>

        <h2>Current User registry settings</h2>

        <xsl:for-each select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser/Key">
          <xsl:call-template name="Registry" />
        </xsl:for-each>

        <h1>Files</h1>

        <xsl:for-each select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Files/*">
          <xsl:call-template name="Folder">
            <xsl:with-param name="Name" select="name(.)" />
          </xsl:call-template>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="KeymanInstalledKeyboards">
    <xsl:param name="Version"/>

    <xsl:variable name="lmroot" select="concat('hkey_local_machine\software\tavultesoft\keyman engine\',$Version,'\installed keyboards')" />
    <xsl:variable name="curoot" select="concat('hkey_current_user\software\tavultesoft\keyman engine\',$Version,'\installed keyboards')" />

    <xsl:if test="
                /TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/LocalMachine//Key[translate(@Path,$uppercase,$lowercase)=$lmroot] or
                /TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$curoot]">
      <h2><xsl:value-of select="$Version"/> Installed Keyboards</h2>

      <table>
        <thead>
          <tr>
            <th>All Users?</th>
            <th>Name</th>
            <th>Filename</th>
            <xsl:if test="$Version!='9.0'">
              <th>KeyboardID</th>
              <th></th>
            </xsl:if>
            <th>Default Language</th>
            <th></th>
            <th>Default Hotkey</th>
            <th>Package</th>
            <th>OSK</th>
          </tr>
        </thead>
        <tbody>
          <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/LocalMachine//Key[translate(@Path,$uppercase,$lowercase)=$lmroot]/Key" mode="KeymanInstalledKeyboards">
            <xsl:with-param name="Version" select="$Version"/>
          </xsl:apply-templates>
          <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/LocalMachine//Key[translate(@Path,$uppercase,$lowercase)=$curoot]/Key" mode="KeymanInstalledKeyboards">
            <xsl:with-param name="Version" select="$Version"/>
          </xsl:apply-templates>
        </tbody>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Key" mode="KeymanInstalledKeyboards">
    <xsl:param name="Version"/>
    <xsl:variable name="name" select="substring(@Path,string-length(../@Path)+2,999)"/>
    <xsl:variable name="filename" select="Value[translate(@Name,$uppercase,$lowercase)='keyman file']/@Value"/>
    <xsl:variable name="keyboardid" select="translate(Value[translate(@Name,$uppercase,$lowercase)='keyman keyboard id']/@Value,$uppercase,$lowercase)"/>
    <xsl:variable name="winkeyboard" select="//Key[translate(@Path,$uppercase,$lowercase)=concat('hkey_local_machine\system\currentcontrolset\control\keyboard layouts\',$keyboardid)]"/>
    <xsl:variable name="languageid" select="translate(Value[translate(@Name,$uppercase,$lowercase)='default language id']/@Value,$uppercase,$lowercase)"/>
    <xsl:variable name="locale" select="//Locale[translate(@ID,$uppercase,$lowercase)=$languageid]"/>

    <tr>
      <td>
        <xsl:choose>
          <xsl:when test="translate(../@Path,$uppercase,$lowercase)='hkey_current_user\software\tavultesoft\keyman engine\8.0\installed keyboards'">No</xsl:when>
          <xsl:otherwise>Yes</xsl:otherwise>
        </xsl:choose>
      </td>
      <td>
        <xsl:value-of select="$name"/>
      </td>
      <td>
        <xsl:call-template name="extractFileName">
          <xsl:with-param name="pTotalString" select="$filename"/>
        </xsl:call-template>
      </td>
      <xsl:if test="$Version!='9.0'">
        <td>
          <xsl:value-of select="$keyboardid"/>
        </td>
        <td>
          <xsl:value-of select="$winkeyboard/Value[translate(@Name,$uppercase,$lowercase)='layout text']/@Value"/>, <xsl:value-of select="$winkeyboard/Value[translate(@Name,$uppercase,$lowercase)='layout id']/@Value"/>
        </td>
      </xsl:if>
      <td>
        <xsl:if test="$languageid and $languageid!='00000000'">
          <xsl:value-of select="$languageid"/>
        </xsl:if>
      </td>
      <td>
        <xsl:if test="$languageid and $languageid!='00000000'">
          <xsl:value-of select="$locale/@Name"/> (<xsl:value-of select="$locale/@Country"/>)
        </xsl:if>
      </td>
      <td>
        <xsl:value-of select="Value[translate(@Name,$uppercase,$lowercase)='keyman default hotkey']/@Value"/>
      </td>
      <td>
        <xsl:value-of select="Value[translate(@Name,$uppercase,$lowercase)='package name']/@Value"/>
      </td>
      <td>
        <xsl:variable name="osk" select="Value[translate(@Name,$uppercase,$lowercase)='visual keyboard']/@Value"/>
        <xsl:call-template name="extractFileName">
          <xsl:with-param name="pTotalString" select="$osk"/>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>

  <xsl:template name="KeymanActiveKeyboards">
    <xsl:param name="Version"/>

    <xsl:variable name="root" select="concat('hkey_current_user\software\tavultesoft\keyman engine\',$Version,'\active keyboards')" />

    <xsl:if test="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$root]">
      <h2><xsl:value-of select="$Version"/> Active Keyboards</h2>

      <table>
        <thead>
          <tr>
            <th>KeymanID</th>
            <th>Name</th>
            <th>Hotkey</th>
            <xsl:if test="$Version != '9.0'">
              <th>LangageID</th>
            </xsl:if>
          </tr>
        </thead>
        <tbody>
          <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$root]/Key" mode="KeymanActiveKeyboards">
            <xsl:with-param name="Version" select="$Version"/>
          </xsl:apply-templates>
        </tbody>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Key" mode="KeymanActiveKeyboards">
    <xsl:param name="Version"/>
    <xsl:variable name="name" select="substring(@Path,string-length(../@Path)+2,999)"/>
    <xsl:variable name="languageid" select="Value[translate(@Name,$uppercase,$lowercase)='keyman installed language']/@Value" />
    <xsl:variable name="keymanid" select="Value[translate(@Name,$uppercase,$lowercase)='keyman id']/@Value"/>
    <xsl:if test="$keymanid">
      <tr>
        <td>
          <xsl:value-of select="$keymanid"/>
        </td>
        <td>
          <xsl:value-of select="$name"/>
        </td>
        <xsl:if test="$Version != '9.0'">
          <td>
            <xsl:value-of select="Value[translate(@Name,$uppercase,$lowercase)='keyman active hotkey']/@Value"/>
          </td>
        </xsl:if>
        <td>
          <xsl:value-of select="$languageid"/>
        </td>
      </tr>
    </xsl:if>
  </xsl:template>

  <xsl:template name="KeymanActiveLanguages">
    <xsl:param name="Version" />
    <xsl:variable name="root"  select="concat('hkey_current_user\software\tavultesoft\keyman engine\',$Version,'\active languages')" />
    <xsl:if test="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$root]">
      <h2><xsl:value-of select="$Version"/> Active Languages</h2>

      <table>
        <thead>
          <tr>
            <th>HKL</th>
            <th>Keyboard Name</th>
          </tr>
        </thead>
        <tbody>
          <xsl:apply-templates select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$root]/Value" mode="KeymanActiveLanguages" />
        </tbody>
      </table>
    </xsl:if>

  </xsl:template>
  <xsl:template match="Value" mode="KeymanActiveLanguages">
    <tr>
      <td>
        <xsl:value-of select="@Name"/>
      </td>
      <td>
        <xsl:value-of select="@Value"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="LanguageProfiles">
    <xsl:param name="Enabled" />
    <xsl:variable name="EnabledText">
      <xsl:choose>
        <xsl:when test="$Enabled">Enabled</xsl:when>
        <xsl:otherwise>Installed</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <h2><xsl:value-of select="$EnabledText" /> Language Profiles</h2>

    <table>
      <thead>
        <tr>
          <th>Language ID</th>
          <th>Language Name</th>
          <th>Description</th>
          <th>Type</th>
          <th>Class</th>
          <th>DLL</th>
          <th>Category</th>
          <th>HKL</th>
          <th>Substitute HKL</th>
          <th>Capabilities</th>
        </tr>
      </thead>
      <tbody>
        <xsl:apply-templates select="LanguageProfile[(@IsEnabled='True' and $Enabled='True') or (not(@IsEnabled) and not($Enabled))]" />
      </tbody>
    </table>
  </xsl:template>

  <xsl:template match="LanguageProfile">
    <tr>
      <td><xsl:value-of select="@LangID"/></td>
      <td>
        <xsl:variable name="langid" select="concat('0000',translate(@LangID,$uppercase,$lowercase))"/>
        <xsl:variable name="locale" select="//Locale[translate(@ID,$uppercase,$lowercase)=$langid]"/>
        <xsl:if test="$locale">
          <xsl:value-of select="$locale/@Name"/> (<xsl:value-of select="$locale/@Country"/>)
        </xsl:if>
      </td>
      <td><xsl:value-of select="@Description"/></td>
      <td><xsl:value-of select="@Type"/></td>
      <td>
        <xsl:choose>
          <xsl:when test="@ClassID='{00000000-0000-0000-0000-000000000000}'"></xsl:when>
          <xsl:when test="@ClassID='{7BA04432-8609-4FE6-BFF7-971091DE0933}'">Keyman Desktop 8</xsl:when>
          <xsl:when test="@ClassID='{487EB753-DB93-48C5-9E6A-4398E777C61D}'">Keyman Desktop 9</xsl:when>
          <xsl:when test="@ClassID='{FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7}'">Keyman Desktop 10-13, Keyman 14+</xsl:when>
          <xsl:when test="@ClassName != ''">
            <xsl:value-of select="@ClassName"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@ClassID"/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
      <td>
        <xsl:value-of select="@ClassFileName"/>
      </td>
      <td>
        <xsl:choose>
          <xsl:when test="@Category='{00000000-0000-0000-0000-000000000000}'"></xsl:when>
          <xsl:otherwise><xsl:value-of select="@Category"/></xsl:otherwise>
        </xsl:choose>
      </td>
      <td>
        <xsl:choose>
          <xsl:when test="@HKL='00000000'"></xsl:when>
          <xsl:otherwise><xsl:value-of select="@HKL"/></xsl:otherwise>
        </xsl:choose>
      </td>
      <td>
        <xsl:choose>
          <xsl:when test="@SubstituteHKL='00000000'"></xsl:when>
          <xsl:otherwise><xsl:value-of select="@SubstituteHKL"/></xsl:otherwise>
        </xsl:choose>
      </td>
      <td>
        <xsl:for-each select="Capabilities/*">
          <xsl:if test="position() &gt; 1">, </xsl:if>
          <xsl:value-of select="name()"/>
        </xsl:for-each>
      </td>
    </tr>
  </xsl:template>

  <xsl:template name="OfficeFullName">
    <xsl:param name="Version" />
    <xsl:choose>
      <xsl:when test="$Version = 9">Office 2000 (9.0)</xsl:when>
      <xsl:when test="$Version = 10">Office XP (10.0)</xsl:when>
      <xsl:when test="$Version = 11">Office 2003 (11.0)</xsl:when>
      <xsl:when test="$Version = 12">Office 2007 (12.0)</xsl:when>
      <xsl:when test="$Version = 13">Office 2010 (13.0)</xsl:when>
      <xsl:when test="$Version = 13">Office 2013 (14.0)</xsl:when>
      <xsl:otherwise><xsl:value-of select="$Version"/></xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="extractFileName">
    <xsl:param name="pTotalString" select="."/>
    <xsl:param name="pPosition" select="1"/>
    <xsl:param name="pLastFound" select="-1"/>
    <xsl:param name="pTotalLength" select="string-length($pTotalString)"/>

    <xsl:choose>
      <xsl:when test="$pPosition > $pTotalLength">
        <xsl:value-of select="substring($pTotalString, $pLastFound + 1)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="vIsDot" select=
       "substring($pTotalString, $pPosition, 1) = '\'"/>

        <xsl:call-template name="extractFileName">
          <xsl:with-param name="pTotalString" select="$pTotalString"/>
          <xsl:with-param name="pTotalLength" select="$pTotalLength"/>
          <xsl:with-param name="pLastFound" select=
        "$pLastFound * not($vIsDot) + $pPosition * $vIsDot"/>
          <xsl:with-param name="pPosition" select="$pPosition+1"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
