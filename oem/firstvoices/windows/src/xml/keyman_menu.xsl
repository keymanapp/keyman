<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:template name="menuframe">
    <script type="text/javascript">
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'keyboardlist', '<xsl:value-of select="$locale/string[@name='S_Keyboards']"/>', '<xsl:value-of select="$locale/string[@name='S_Keyboards_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'options', '<xsl:value-of select="$locale/string[@name='S_Options']"/>', '<xsl:value-of select="$locale/string[@name='S_Options_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'hotkeys', '<xsl:value-of select="$locale/string[@name='S_Hotkeys']"/>', '<xsl:value-of select="$locale/string[@name='S_Hotkeys_AccessChar']"/>');
          menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'support', '<xsl:value-of select="$locale/string[@name='S_Support']"/>', '<xsl:value-of select="$locale/string[@name='S_Support_AccessChar']"/>');
          //menuframe_add('<xsl:value-of select="/Keyman/templatepath"/>', 'keepintouch', '<xsl:value-of select="$locale/string[@name='S_KeepInTouch']"/>', '<xsl:value-of select="$locale/string[@name='S_KeepInTouch_AccessChar']"/>');
    </script>
    <span class="menuframe" id="menuframe_footer"><xsl:text></xsl:text></span>
  </xsl:template>

</xsl:stylesheet>
