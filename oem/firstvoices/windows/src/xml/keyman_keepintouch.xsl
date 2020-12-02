<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template name="content_keepintouch">
  
    <script>
      window['menuframe_activate_keepintouch'] = function() {
        document.getElementById('keepintouch_frame').src = 'https://keyman.com/go/desktop/10.0/keep-in-touch?embed=1';
      }
    </script>
  
    <div class="header">
      <xsl:call-template name="header_helplinks" />
      <xsl:value-of select="$locale/string[@name='S_KeepInTouch']"/>
    </div>
        
    <div id="subcontent_keepintouch" class="content">
      <div id="keepintouch_content">
        <iframe id='keepintouch_frame'></iframe>
      </div>
    </div>
  </xsl:template>	
</xsl:stylesheet>