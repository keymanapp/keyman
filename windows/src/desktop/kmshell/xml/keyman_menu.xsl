<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <xsl:template name="menuframe">
    <script type="text/javascript">
          menuframe_add('/app/', 'keyboardlist', '<xsl:value-of select="$locale/String[@Id='S_Keyboards']"/>', '<xsl:value-of select="$locale/String[@Id='S_Keyboards_AccessChar']"/>');
          menuframe_add('/app/', 'options', '<xsl:value-of select="$locale/String[@Id='S_Options']"/>', '<xsl:value-of select="$locale/String[@Id='S_Options_AccessChar']"/>');
          menuframe_add('/app/', 'hotkeys', '<xsl:value-of select="$locale/String[@Id='S_Hotkeys']"/>', '<xsl:value-of select="$locale/String[@Id='S_Hotkeys_AccessChar']"/>');
          menuframe_add('/app/', 'support', '<xsl:value-of select="$locale/String[@Id='S_Support']"/>', '<xsl:value-of select="$locale/String[@Id='S_Support_AccessChar']"/>');
          menuframe_add('/app/', 'keepintouch', '<xsl:value-of select="$locale/String[@Id='S_KeepInTouch']"/>', '<xsl:value-of select="$locale/String[@Id='S_KeepInTouch_AccessChar']"/>');
    </script>
    <span class="menuframe" id="menuframe_footer"><xsl:text></xsl:text></span>
  </xsl:template>

</xsl:stylesheet>
