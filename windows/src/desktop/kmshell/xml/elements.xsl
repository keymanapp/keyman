<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:msxsl="urn:schemas-microsoft-com:xslt">

  <xsl:output method="html" version="1.0" encoding="utf-8" omit-xml-declaration="yes" standalone="yes" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />

  <!-- I978 - locale was not reliably selected due to undefined order of document() loading and | processing -->
  <xsl:variable name="prilocale" select="document(/Keyman/localepath)/Locale" />
  <xsl:variable name="altlocale" select="document('strings.xml')/resources/*[not(@name = $prilocale//@name)]" />
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
    <xsl:param name="className" />
		<xsl:param name="default" />
    <xsl:param name="width" />
    <xsl:param name="id" />
    <xsl:param name="tabid" />
    <xsl:param name="shield" />

    <xsl:choose>
      <xsl:when test="$shield = 1 and /Keyman/canelevate">
        <button>
          <xsl:attribute name="type">
            <xsl:choose>
              <xsl:when test="$default=1">submit</xsl:when>
              <xsl:otherwise>button</xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:attribute name="id">button_<xsl:value-of select="$id"/></xsl:attribute>
          <xsl:attribute name="onclick"><xsl:choose><xsl:when test="$onclick != ''"><xsl:value-of select="$onclick"/></xsl:when><xsl:otherwise>location.href="<xsl:value-of select="$command"/>";</xsl:otherwise></xsl:choose></xsl:attribute>
          <xsl:attribute name="tabindex"><xsl:value-of select="$tabid"/></xsl:attribute>
          <xsl:attribute name="style">
            font-size: 12px;
            height: 25px;
            display: inline-block;
            text-align: center;
            width: <xsl:value-of select="$width"/>;
            margin-right: 8px;
            vertical-align: top;
          </xsl:attribute>
          <img alt="" style="vertical-align: middle; width: 16px; padding: 0; margin: 2px 4px 5px 0px; display: inline; height: 18px;" src="/app/shield.png" />
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
          <xsl:attribute name="ID">button_<xsl:value-of select="$id"/></xsl:attribute>
          <xsl:attribute name="onclick"><xsl:choose><xsl:when test="$onclick != ''"><xsl:value-of select="$onclick"/></xsl:when><xsl:otherwise>location.href="<xsl:value-of select="$command"/>";</xsl:otherwise></xsl:choose></xsl:attribute>
          <xsl:attribute name="value"><xsl:value-of select="$caption" /></xsl:attribute>
          <xsl:attribute name="tabindex"><xsl:value-of select="$tabid"/></xsl:attribute>
          <xsl:attribute name="style">
            width: <xsl:value-of select="$width"/>;
            font-size: 11px;
            height: 25px;
            display: inline-block;
            margin-right: 8px;
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

</xsl:stylesheet>
