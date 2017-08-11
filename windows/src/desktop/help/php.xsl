<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="../../ext/docbook/html/docbook.xsl"/>

  <xsl:import href="php-chunk-common.xsl"/>

  <xsl:include href="../../ext/docbook/html/chunk-code.xsl"/>
  
  
  <xsl:param name="html.stylesheet" select="'kmhelp.css'"/>
  
  <xsl:param name="base.dir" select="'../../../bin/help/php/desktop/'" />

  <xsl:param name="use.id.as.filename" select="1" />
  <xsl:param name="html.ext" select="'.php'" />
  
  <xsl:param name="ulink.target" select="'_blank'" />

  <xsl:param name="chunker.output.encoding" select="utf-8" />
  <xsl:param name="chunk.section.depth" select="6" />
  <xsl:param name="chunk.first.sections" select="1" />
  <xsl:param name="chunk.fast" select="1" />

  <xsl:param name="generate.id.attributes" select="1" />

  <xsl:param name="toc.max.depth" select="1" />
  <xsl:param name="generate.section.toc.level" select="6" />
  
  <!-- Support for segmented graphics -->

  <xsl:template match="table[@class='segmentedgraphic']" mode="class.value">
     <xsl:value-of select="'segmentedgraphic'"/>
  </xsl:template>

  <xsl:param name="local.l10n.xml" select="document('')"/>
  <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
    <l:l10n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0" language="en">
     <l:context name="xref">
        <l:template name="chapter" text="Chapter %n, %t"/>
        <l:template name="section" text="%t"/>
      </l:context>
    </l:l10n>
  </l:i18n>
  
  <xsl:template match="redirect">
    <meta http-equiv="refresh"><xsl:attribute name='content'>0;url=<xsl:value-of select="@url" />.php</xsl:attribute></meta>
    <para>If you are not automatically redirected to the topic, please <a><xsl:attribute name='href'><xsl:value-of select="@url" />.php</xsl:attribute>click here</a>.</para>
  </xsl:template>

</xsl:stylesheet>