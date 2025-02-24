<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="base.xslt" />

  <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

  <xsl:output method="html" encoding="utf-16" />

  <xsl:template match="/">
    <html>
      <xsl:call-template name="head" />
      <body>
        <h1>Loaded Keyboard Layouts</h1>
        <table>
          <thead>
            <tr>
              <th>ID</th>
              <th>HKL</th>
              <th>Type</th>
              <th>Language</th>
              <th>Country</th>
              <th>Keyboard</th>
              <th>Filename</th>
              <th>Keyman Keyboard</th>
              <th>Substitute?</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Preload']/Value" />
          </tbody>
        </table>

        <h1>User Profile Options</h1>
        <table>
          <thead>
            <tr>
              <th>Languages</th>
              <th>ShowAutoCorrection</th>
              <th>ShowTextPrediction</th>
              <th>ShowCasing</th>
              <th>ShowShiftLock</th>
              <th>InputMethodOverride</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Key[@Path='HKEY_CURRENT_USER\Control Panel\International\User Profile']/Value"/>
          </tbody>
        </table>

        <h1>BCP47 Codes</h1>
        <table>
          <thead>
            <tr>
              <th>BCP47 Code</th>
              <th>Transient LangId</th>
              <th>Cached Language Name</th>
              <th>{LangID}:{Keyboard Layout ID}</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Key[@Path='HKEY_CURRENT_USER\Control Panel\International\User Profile']/Key"/>
          </tbody>
        </table>

        <h1>Windows Keyboard Hotkeys</h1>
        <table>
          <thead>
            <tr>
              <th>Action</th>
              <th>Hotkey</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Toggle']/Value"/>
          </tbody>
        </table>

        <h1>System Locales</h1>
        <table>
          <thead>
            <tr>
              <th>ID</th>
              <th>Language</th>
              <th>Country</th>
              <th>Flags</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Locale" />
          </tbody>
        </table>

        <h1>System Keyboard Layouts</h1>
        <table>
          <thead>
            <tr>
              <th>ID</th>
              <th>File</th>
              <th>Name</th>
              <th>Notes</th>
            </tr>
          </thead>
          <tbody>
            <xsl:apply-templates select="//Key[@Path='HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\keyboard layouts']/Key" />
          </tbody>
        </table>

        <h1>Uniscribe</h1>
      </body>
    </html>
</xsl:template>

<xsl:template match="//Key[@Path='HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\keyboard layouts']/Key">
  <tr>
    <td><xsl:value-of select="substring-after(@Path, 'keyboard layouts\')"/></td>
    <td><xsl:value-of select="Value[@Name='Layout File' or @Name='layout file']/@Value"/></td>
    <td><xsl:value-of select="Value[@Name='Layout Text' or @Name='layout text']/@Value"/></td>
    <td><xsl:if test="Value[@Name='keyman install' or @Name='Keyman Install']">Keyman system shadow keyboard</xsl:if></td>
  </tr>
</xsl:template>
<xsl:template match="//Locale">
  <tr>
    <td><xsl:value-of select="@ID"/></td>
    <td><xsl:value-of select="@Name"/></td>
    <td><xsl:value-of select="@Country"/></td>
    <td><xsl:value-of select="@Flags"/></td>
  </tr>
</xsl:template>

<xsl:template match="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Toggle']/Value">
  <tr>
    <td><xsl:value-of select="@Name"/></td>
    <td>
      <xsl:choose>
        <xsl:when test="@Value = '1'">Alt+Left Shift</xsl:when>
        <xsl:when test="@Value = '2'">Ctrl+Shift</xsl:when>
        <xsl:when test="@Value = '3'">None</xsl:when>
        <xsl:when test="@Value = '4'">Grave accent (`)</xsl:when>
        <xsl:otherwise>Unknown (<xsl:value-of select="@Value" />)</xsl:otherwise>
      </xsl:choose>
    </td>
  </tr>
</xsl:template>

<xsl:template match="//Key[@Path='HKEY_CURRENT_USER\Control Panel\International\User Profile']/Value">
  <tr>
    <td><xsl:value-of select="@Name"/></td>
    <td><xsl:value-of select="@Value"/></td>
  </tr>
</xsl:template>

<xsl:template match="//Key[@Path='HKEY_CURRENT_USER\Control Panel\International\User Profile']/Key">
  <tr>
    <td><xsl:value-of select="substring-after(@Path, 'User Profile\')"/></td>
    <td><xsl:value-of select="Value[@Name='TransientLangId']/@Value"/></td>
    <td><xsl:value-of select="Value[@Name='CachedLanguageName']/@Value"/></td>
    <td>
      <xsl:for-each select="Value[contains('0123456789', substring(@Name,1,1))]">
        <xsl:value-of select="@Name"/>
        <xsl:if test="position() != last()">, </xsl:if>
      </xsl:for-each>
    </td>
  </tr>
</xsl:template>

<xsl:template match="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Preload']/Value">
  <xsl:variable name="LanguageID">0000<xsl:value-of select="substring(@Value,5,4)"/></xsl:variable>
  <xsl:variable name="KeyboardID">
    <xsl:choose>
      <xsl:when test="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Substitutes']/Value[@Name=current()/@Value]"><xsl:value-of select="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Substitutes']/Value[@Name=current()/@Value]/@Value"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="@Value"/></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="KeyboardRegistryKey" select="//Key[translate(@Path,$uppercase,$lowercase)=translate(concat('HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\keyboard layouts\',$KeyboardID),$uppercase,$lowercase)]"/>
  <xsl:variable name="KeyboardName" select="$KeyboardRegistryKey/Value[translate(@Name,$uppercase,$lowercase)='layout text']/@Value" />
  <xsl:variable name="Language" select="//Locale[@ID=$LanguageID]" />

  <xsl:variable name="LayoutFile" select="$KeyboardRegistryKey/Value[translate(@Name,$uppercase,$lowercase)='layout file']/@Value" />
  <xsl:variable name="LayoutID" select="$KeyboardRegistryKey/Value[translate(@Name,$uppercase,$lowercase)='layout id']/@Value" />

  <xsl:variable name="KeyboardIDFirst" select="translate(substring($KeyboardID,1,1),$uppercase,$lowercase)"/>
  <xsl:variable name="HKL1">
    <xsl:choose>
      <xsl:when test="$KeyboardIDFirst='a'"><xsl:value-of select="concat('f',substring($LayoutID,2,3),substring($LanguageID,5,4))"/></xsl:when><!-- subst with layout id -->
      <xsl:when test="$KeyboardIDFirst='e'"><xsl:value-of select="$KeyboardID"/></xsl:when><!-- ime -->
      <xsl:otherwise><xsl:value-of select="concat(substring($KeyboardID,5,4),substring($LanguageID,5,4))"/></xsl:otherwise><!-- basic keyboard -->
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="HKL" select="translate($HKL1,$uppercase,$lowercase)"/>

  <xsl:variable name="keyman_activelangs_root" select="'hkey_current_user\software\tavultesoft\keyman engine\8.0\active languages'" />
  <xsl:variable name="keyman_activelangs" select="/TavultesoftSystemInformation/TSI_Keyman/KeymanDiagnosticRecord/Keyman/Registry/CurrentUser//Key[translate(@Path,$uppercase,$lowercase)=$keyman_activelangs_root]" />

  <xsl:variable name="KeymanKeyboard" select="$keyman_activelangs/Value[translate(@Name,$uppercase,$lowercase)=$HKL]/@Value" />

  <xsl:variable name="Type">
    <xsl:choose>
      <xsl:when test="$KeyboardRegistryKey/Value[translate(@Name,$uppercase,$lowercase)='keyman install']/@Value = 1">Keyman System Shadow Keyboard</xsl:when>
      <xsl:when test="$KeymanKeyboard">Keyman Keyboard</xsl:when>
      <xsl:otherwise>Windows Keyboard</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <tr>
    <td><xsl:value-of select="@Value"/></td>
    <td><xsl:value-of select="$HKL"/></td>
    <td><xsl:value-of select="$Type"/></td>
    <td><xsl:value-of select="$Language/@Name"/></td>
    <td><xsl:value-of select="$Language/@Country"/></td>
    <td><xsl:value-of select="$KeyboardName"/></td>
    <td><xsl:value-of select="$LayoutFile"/></td>
    <td><xsl:value-of select="$KeymanKeyboard"/></td>
    <td>
      <xsl:choose>
        <xsl:when test="//Key[@Path='HKEY_CURRENT_USER\keyboard layout\Substitutes']/Value[@Name=current()/@Value]">Yes</xsl:when>
        <xsl:otherwise>No</xsl:otherwise>
      </xsl:choose>
    </td>
  </tr>
</xsl:template>

</xsl:stylesheet>
