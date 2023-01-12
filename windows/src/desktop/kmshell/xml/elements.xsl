<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt">

  <xsl:output method="html" version="1.0" encoding="utf-8" omit-xml-declaration="yes" standalone="yes" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />

  <!-- I978 - locale was not reliably selected due to undefined order of document() loading and | processing -->
  <xsl:variable name="prilocale" select="document(concat(/Keyman/localeserver,/Keyman/locale))/resources" />
  <xsl:variable name="altlocale" select="document(concat(/Keyman/localeserver,'en'))/resources/*[not(@name = $prilocale//@name)]" />
  <xsl:variable name="comlocale">
      <xsl:copy-of select="$prilocale/*" />
      <xsl:copy-of select="$altlocale" />
  </xsl:variable>
  <xsl:variable name="dialoginfo" select="document('dialoginfo.xml')/DialogInfo" />
  <xsl:variable name="locale" select="msxsl:node-set($comlocale)" />

  <!--
  -
  - Generic list implementation: supports entry plus expanding details
  -
  -->



  <!--
  -
  - Checkbox implementation
  -
  -->

  <xsl:template name="checkbox">
    <xsl:param name="id" />
    <xsl:param name="checked" />
    <xsl:param name="onclick" />
    <xsl:param name="onmousedown" />
    <xsl:param name="title" />
    <xsl:param name="disabled" />
    <xsl:param name="tabid" />

    <input type="checkbox">
      <xsl:attribute name="tabindex"><xsl:value-of select="$tabid"/></xsl:attribute>
      <xsl:attribute name="id"><xsl:value-of select="$id"/></xsl:attribute>
      <xsl:if test="$checked = 'true'"><xsl:attribute name="checked"></xsl:attribute></xsl:if>
      <xsl:if test="$disabled = 'true'"><xsl:attribute name="disabled">true</xsl:attribute></xsl:if>
      <xsl:if test="$title"><xsl:attribute name="title"><xsl:value-of select="$title"/></xsl:attribute></xsl:if>
      <xsl:attribute name="onclick"><xsl:value-of select="$onclick"/></xsl:attribute>
      <xsl:if test="$onmousedown"></xsl:if><xsl:attribute name="onmousedown"><xsl:value-of select="$onmousedown"/></xsl:attribute>
    </input>
  </xsl:template>

  <!--
  -
  - Button implementation
  -
  -->

  <xsl:template name="button">
    <xsl:param name="caption" />
    <xsl:param name="command" />
    <xsl:param name="onclick" />
    <xsl:param name="onkeydown" />
    <xsl:param name="className" />
		<xsl:param name="default" />
    <xsl:param name="width" />
    <xsl:param name="id" />
    <xsl:param name="disabled" />
    <xsl:param name="tabid" />
    <xsl:param name="shield" />
    <xsl:param name="visible" />
    <xsl:param name="background" />
    <xsl:param name="color" />
    <xsl:param name="fontweight" />

    <xsl:choose>
      <xsl:when test="$shield = 1 and /Keyman/canelevate">
        <button>
          <xsl:attribute name="type">
            <xsl:choose>
              <xsl:when test="$default=1">submit</xsl:when>
              <xsl:otherwise>button</xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:if test="$disabled = 1"><xsl:attribute name="disabled">disabled</xsl:attribute></xsl:if>
          <xsl:attribute name="id">button_<xsl:value-of select="$id"/></xsl:attribute>
          <xsl:attribute name="class"><xsl:value-of select="$className"/></xsl:attribute>
          <xsl:attribute name="onclick"><xsl:choose><xsl:when test="$onclick != ''"><xsl:value-of select="$onclick"/></xsl:when><xsl:otherwise>location.href="<xsl:value-of select="$command"/>";</xsl:otherwise></xsl:choose></xsl:attribute>
          <xsl:attribute name="onkeydown"><xsl:choose><xsl:when test="$onkeydown != ''"><xsl:value-of select="$onkeydown"/></xsl:when></xsl:choose></xsl:attribute>
          <xsl:attribute name="tabindex"><xsl:value-of select="$tabid"/></xsl:attribute>
          <xsl:attribute name="style">
            font-size: 11px;
            <xsl:choose><xsl:when test="$fontweight != ''">font-weight: <xsl:value-of select="$fontweight"/>;</xsl:when></xsl:choose>
            height: 25px;
            display: inline-block;
            text-align: center;
            <xsl:choose><xsl:when test="$width != ''">width: <xsl:value-of select="$width"/>;</xsl:when></xsl:choose>
            margin-right: 8px;
            vertical-align: top;
            padding-top: 5px;
            <xsl:if test="$visible = 'false'">display:none;</xsl:if>
            <xsl:if test="string($background)">background:<xsl:value-of select="$background"/>;</xsl:if>
            <xsl:if test="string($color)">color:<xsl:value-of select="$color"/>;</xsl:if>
          </xsl:attribute>
          <img alt="" style="vertical-align: middle; width: 16px; padding: 0; margin: 0px 2px 2px 0px; display: inline; height: 16px;" src="/app/shield.png" />
          <xsl:value-of select="$caption" />
        </button>
      </xsl:when>
      <xsl:otherwise>
        <input>
          <xsl:attribute name="type">
            <xsl:choose>
              <xsl:when test="$default=1">submit</xsl:when>
              <xsl:otherwise>button</xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:if test="$disabled = 1"><xsl:attribute name="disabled">disabled</xsl:attribute></xsl:if>
          <xsl:attribute name="ID">button_<xsl:value-of select="$id"/></xsl:attribute>
          <xsl:attribute name="class"><xsl:value-of select="$className"/></xsl:attribute>
          <xsl:attribute name="onclick"><xsl:choose><xsl:when test="$onclick != ''"><xsl:value-of select="$onclick"/></xsl:when><xsl:otherwise>location.href="<xsl:value-of select="$command"/>";</xsl:otherwise></xsl:choose></xsl:attribute>
          <xsl:attribute name="onkeydown"><xsl:choose><xsl:when test="$onkeydown != ''"><xsl:value-of select="$onkeydown"/></xsl:when></xsl:choose></xsl:attribute>
          <xsl:attribute name="value"><xsl:value-of select="$caption" /></xsl:attribute>
          <xsl:attribute name="tabindex"><xsl:value-of select="$tabid"/></xsl:attribute>
          <xsl:attribute name="style">
            <xsl:choose><xsl:when test="$width != ''">width: <xsl:value-of select="$width"/>;</xsl:when></xsl:choose>
            font-size: 11px;
            <xsl:choose><xsl:when test="$fontweight != ''">font-weight: <xsl:value-of select="$fontweight"/>;</xsl:when></xsl:choose>
            height: 25px;
            display: inline-block;
            margin-right: 8px;
            <xsl:if test="$visible = 'false'">display:none;</xsl:if>
            <xsl:if test="string($background)">background:<xsl:value-of select="$background"/>;</xsl:if>
            <xsl:if test="string($color)">color:<xsl:value-of select="$color"/>;</xsl:if>
          </xsl:attribute>
        </input>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!--
  -
  - Popup menu implementation
  -
  -->

  <xsl:template name="menuitem">
    <xsl:param name="caption" />
    <xsl:param name="command" />
    <xsl:param name="id" />
    <span class="menuitem" tabindex="1">
      <xsl:attribute name="id"><xsl:value-of select="$id" /></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:menuitemover();</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:menuitemout();</xsl:attribute>
      <xsl:attribute name="onmouseup">javascript:menuitemdown(false);</xsl:attribute>
      <xsl:attribute name="onfocus">javascript:menuitemover();</xsl:attribute>
      <xsl:attribute name="onblur">javascript:menuitemout();</xsl:attribute>
      <xsl:attribute name="onkeydown">javascript:return menuitem_keydown(0);</xsl:attribute>
      <xsl:attribute name="command"><xsl:value-of select="$command" /></xsl:attribute>
      <xsl:attribute name="onclick">javascript:HideMenu();return menuitemdown(true);</xsl:attribute>
      <xsl:copy-of select="$caption"/>
    </span>
  </xsl:template>

  <xsl:template name="menubutton">
    <xsl:param name="caption" />
    <xsl:param name="menutemplate" />
    <xsl:param name="id" />
    <xsl:param name="align" />
    <xsl:param name="width" />
    <xsl:param name="className" />
    <xsl:param name="tabid" />

    <xsl:call-template name="button">
      <xsl:with-param name="caption"><xsl:value-of select="$caption" /></xsl:with-param>
      <xsl:with-param name="onclick">ShowMenu('<xsl:value-of select="$id"/>','<xsl:value-of select="$align" />',0,0); return false;</xsl:with-param>
      <xsl:with-param name="command">javascript:ShowMenu('<xsl:value-of select="$id"/>','<xsl:value-of select="$align" />',0,0);</xsl:with-param>
      <xsl:with-param name="id"><xsl:value-of select="$id"/></xsl:with-param>
      <xsl:with-param name="className"><xsl:value-of select="$className"/></xsl:with-param>
      <xsl:with-param name="width"><xsl:value-of select="$width"/></xsl:with-param>
      <xsl:with-param name="tabid"><xsl:value-of select="$tabid"/></xsl:with-param>
    </xsl:call-template>

  </xsl:template>

  <!-- Replaces a string, e.g. ' with \' https://stackoverflow.com/a/7712434/1836776 -->

  <xsl:template name="escape-apos">
    <xsl:param name="text"/>
    <xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="$text" />
      <xsl:with-param name="replace" select='"&apos;"' />
      <xsl:with-param name="with" select='"\&apos;"' />
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="with"/>
    <xsl:choose>
      <xsl:when test="contains($text,$replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$with"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text"
                          select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="with" select="$with"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<!-- Hex to Decimal https://stackoverflow.com/questions/22905134/convert-a-hexadecimal-number-to-an-integer-in-xslt-->

  <xsl:template name="hex2dec">
    <xsl:param name="hex"/>
    <xsl:param name="dec" select="0"/>
    <xsl:param name="MSB" select="translate(substring($hex, 1, 1), 'abcdef', 'ABCDEF')"/>
    <xsl:param name="value" select="string-length(substring-before('0123456789ABCDEF', $MSB))"/>
    <xsl:param name="result" select="16 * $dec + $value"/>
    <xsl:choose>
        <xsl:when test="string-length($hex) > 1">
            <xsl:call-template name="hex2dec">
                <xsl:with-param name="hex" select="substring($hex, 2)"/>
                <xsl:with-param name="dec" select="$result"/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="$result"/>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

</xsl:stylesheet>
