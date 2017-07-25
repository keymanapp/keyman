<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="../../../cmpsrc/docbook/html/chunk.xsl"/>
  <xsl:param name="html.stylesheet" select="'kmhelp.css'"/>
  
  <xsl:param name="base.dir" select="'../../../bin/help/desktop/'" />

  <xsl:param name="use.id.as.filename" select="1" />
  <xsl:param name="html.ext" select="'.html'" />
  
  <xsl:param name="ulink.target" select="'_blank'" />

<!--  <xsl:param name="chunker.output.encoding" select="utf-8" />-->
  <xsl:param name="chunk.section.depth" select="6" />
  <xsl:param name="chunk.first.sections" select="1" />
  <xsl:param name="chunk.fast" select="1" />

  <xsl:param name="generate.id.attributes" select="1" />

  <xsl:param name="toc.max.depth" select="1" />
  <xsl:param name="generate.section.toc.level" select="6" />

  <xsl:variable name="topicresponse_xml" select="document('topicresponse.xml',/)"/>
  
  <xsl:template name="user.head.content">
     <xsl:copy-of select="$topicresponse_xml/html/head/node()"/>
  </xsl:template>

  <xsl:template name="user.footer.navigation">
     <xsl:copy-of select="$topicresponse_xml/html/body/node()"/>
  </xsl:template>
  
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
  
</xsl:stylesheet>