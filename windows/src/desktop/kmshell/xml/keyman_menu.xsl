<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <xsl:template name="menuframe">
    <!-- we have to escape ' character as we embed in Javascript -->
    <xsl:variable name="locale_Keyboards"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Keyboards']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Options"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Options']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Hotkeys"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Hotkeys']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Support"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Support']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Update"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Update']" /></xsl:call-template></xsl:variable>

    <xsl:variable name="locale_Keyboards_AccessChar"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Keyboards_AccessChar']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Options_AccessChar"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Options_AccessChar']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Hotkeys_AccessChar"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Hotkeys_AccessChar']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Support_AccessChar"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Support_AccessChar']" /></xsl:call-template></xsl:variable>
    <xsl:variable name="locale_Update_AccessChar"><xsl:call-template name="escape-apos"><xsl:with-param name="text" select="$locale/string[@name='S_Update_AccessChar']" /></xsl:call-template></xsl:variable>

    <script type="text/javascript">
          menuframe_add('/app/', 'keyboardlist', '<xsl:value-of select="$locale_Keyboards"/>', '<xsl:value-of select="$locale_Keyboards_AccessChar"/>');
          menuframe_add('/app/', 'options', '<xsl:value-of select="$locale_Options"/>', '<xsl:value-of select="$locale_Options_AccessChar"/>');
          menuframe_add('/app/', 'hotkeys', '<xsl:value-of select="$locale_Hotkeys"/>', '<xsl:value-of select="$locale_Hotkeys_AccessChar"/>');
          menuframe_add('/app/', 'support', '<xsl:value-of select="$locale_Support"/>', '<xsl:value-of select="$locale_Support_AccessChar"/>');
          menuframe_add('/app/', 'update', '<xsl:value-of select="$locale_Update"/>', '<xsl:value-of select="$locale_Update_AccessChar"/>');
    </script>
    <span class="menuframe" id="menuframe_footer"><xsl:text></xsl:text></span>
  </xsl:template>

</xsl:stylesheet>
