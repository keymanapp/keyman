<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE xsl:stylesheet[
  <!ENTITY footerheight "40">
  <!ENTITY menuwidth "118">
  <!ENTITY headerheight "40">
]>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  <xsl:output method="xml" version="1.0" encoding="utf-8" doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd" />

  <xsl:include href="elements.xsl"/>

  <xsl:include href="menu.xsl"/>

  <xsl:include href="keyboardlist.xsl"/>
  <xsl:include href="options.xsl"/>
  <xsl:include href="hotkeys.xsl"/>
  <xsl:include href="addins.xsl"/>
  <xsl:include href="languages.xsl"/>
  <xsl:include href="support.xsl"/>

  <xsl:include href="footer.xsl"/>
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:k="http://www.tavultesoft.com/xml/70">
      <xsl:call-template name="head">
        <xsl:with-param name="title">Keyman Configuration</xsl:with-param>
      </xsl:call-template>
      <body style="filter: progid:DXImageTransform.Microsoft.Gradient(gradientType=0,startColorStr=#fafafa,endColorStr=Gainsboro);">
        <div id="menuframe">
          <xsl:call-template name="menuframe" />
        </div>
        <div id="contentframe">
          <div class="contentpage" id="content_keyboardlist"><xsl:call-template name="content_keyboardlist" /></div>
          <div class="contentpage" id="content_options"><xsl:call-template name="content_options" /></div>
          <div class="contentpage" id="content_hotkeys"><xsl:call-template name="content_hotkeys" /></div>
          <div class="contentpage" id="content_addins"><xsl:call-template name="content_addins" /></div>
          <div class="contentpage" id="content_languages"><xsl:call-template name="content_languages" /></div>
          <div class="contentpage" id="content_support"><xsl:call-template name="content_support" /></div>
        </div>
        <div id="footerframe">
          <xsl:call-template name="footerframe" />
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template name="head">
    <xsl:param name="title" />
    <head>
      <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
      <title><xsl:value-of select="$title"/></title>
      <style type="text/css">
        #menuframe {
          position: absolute;
          left: 0; 
          top: 38;
          width: &menuwidth;px;
          height: expression(document.body.clientHeight-&footerheight;-&headerheight;+2);
          overflow: hidden;
          background: none;
        }
        #contentframe {
          position: absolute;
          left: &menuwidth;px;
          top: 0;
          width: expression(document.body.clientWidth-&menuwidth;);
          height: expression(document.body.clientHeight-&footerheight;);
          overflow: hidden;
          x-background: #FFFFFF;
          border-bottom: 2px solid #888888; 
        }
        #footerframe {
          position: absolute;
          left: 0px;
          top: expression(document.body.clientHeight-&footerheight;);
          height: &footerheight;px;
          width: 100%;
          overflow: hidden;
          background: none;
          padding: 8px 3px 8px 3px;
        }

        .contentpage { width: 100%; height: 100%; display: none; }

        .header {
          position: absolute;
          left: 0;
          top: 0;
          width: 100%;
          height: &headerheight;px;
          overflow: hidden;
          font: 18pt Tahoma;
          padding: 4px;
          background: none;
          border-bottom: 2px solid #888888; 
        }
        
        .content {
          background: white;
          position: absolute; 
          left: 0; 
          top: &headerheight;px;
          width: 100%; 
          height: expression(
            (document.getElementById('contentframe') ?
              document.getElementById('contentframe').offsetHeight : 300) - &headerheight;
          ); 
          overflow: hidden; 
          font: 9pt Tahoma;
          border-right: 2px solid #888888; 
        }
        
        body { padding: 0px; margin: 0px; overflow: hidden; width: 100% }
        html { width: 100%; padding: 0px; margin: 0px }

        <xsl:call-template name="element_style" />

        <xsl:call-template name="menuframe_style" />
        
        <xsl:call-template name="content_keyboardlist_style" />
        <xsl:call-template name="content_options_style" />
        <xsl:call-template name="content_hotkeys_style" />
        <xsl:call-template name="content_addins_style" />
        <xsl:call-template name="content_languages_style" />
        <xsl:call-template name="content_support_style" />

        <xsl:call-template name="footerframe_style" />
      </style>
      <xsl:call-template name="element_head" />
    </head>
  </xsl:template>
  
</xsl:stylesheet>