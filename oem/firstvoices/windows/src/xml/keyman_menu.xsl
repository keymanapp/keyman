<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template name="menuframe_style">
    .menuframe { width: <xsl:value-of select="$locale_keyman/menuwidth"/>; display: block; text-align: center; font-size: 12px; padding: 8px 0px 8px 2px; border-right: 2px solid #888888; background: none; }
    .menuframe_active { width: 100%; display: block; text-align: center; font-size: 12px; padding: 6px 2px 6px 0px;
         background: white; border-top: 2px solid #888888; border-bottom: 2px solid #888888; border-left: 2px solid #888888; }
  </xsl:template>
  
  <xsl:template name="menuframe">
    <script type="text/javascript">
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'keyboardlist', '<xsl:value-of select="$locale/String[@Id='S_Keyboards']"/>', '<xsl:value-of select="$locale/String[@Id='S_Keyboards_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'options', '<xsl:value-of select="$locale/String[@Id='S_Options']"/>', '<xsl:value-of select="$locale/String[@Id='S_Options_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'hotkeys', '<xsl:value-of select="$locale/String[@Id='S_Hotkeys']"/>', '<xsl:value-of select="$locale/String[@Id='S_Hotkeys_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'support', '<xsl:value-of select="$locale/String[@Id='S_Support']"/>', '<xsl:value-of select="$locale/String[@Id='S_Support_AccessChar']"/>');
    </script>
    <span class="menuframe" id="menuframe_footer"><xsl:text></xsl:text></span>
  </xsl:template>

</xsl:stylesheet>
