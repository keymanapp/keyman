<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <html>
      <head>
        <meta charset="UTF-8" />
        <script src="/app/lib/sentry/bundle.min.js"><xsl:text> </xsl:text></script>
        <script src="/app/lib/sentry/init.js"><xsl:text> </xsl:text></script>

        <title>Help</title>
        <style>
          .title {
            font: bold 8pt Tahoma;
            margin-bottom: 4px;
          }
          .control {
            display: none;
            font: 8pt Tahoma;
          }
          .morehelp { padding-top: 4px; display: inline-block; font: bold 8pt Tahoma }
          .code { font: "Lucida Console"; border: solid 1px #808080; background-color: #e0e0e0; margin: 6px 12px; padding: 2px}
          p { padding: 0px; margin: 2px }
          body { margin: 0; padding: 8px; background-color: #F4F4D0; overflow: auto }
        </style>
        <script type="text/javascript">

            var LastElem = null;
            function ActivatePage(frm, ctrl) {
              //frm = frm.replace(/[#/]/g, '_');
              var id = frm.substr(0, 'language/'.length) == 'language/' ?
                frm :
                frm + '-' + ctrl;

              var elem = document.getElementById(id);
              if(!elem) {
                var wildcard_id = frm + '-*';
                elem = document.getElementById(wildcard_id);
              }
              if(!elem) {
                elem = document.getElementById('nohelp_name');
                elem.innerHTML = frm + ' - ' + ctrl;
                elem = document.getElementById('nohelp');
                var e = document.getElementById('nohelp_link');
                e.href='help:'+frm;
                location.href = 'missing:'+frm+':'+ctrl;
              } else {
                location.href = 'found:'+frm+':'+ctrl;
              }
              if(LastElem) LastElem.style.display = 'none';
              LastElem = elem;
              elem.style.display = 'inline-block';
            }
          </script>
      </head>
      <body>
        <xsl:apply-templates select="//LanguageReference" />
        <xsl:apply-templates select="//Control" />
        <div class="control" id="nohelp">
          Context help is not available for <b><span id="nohelp_name"></span></b>.
          <div class="morehelp"><a id='nohelp_link' href="help:">Look for online help for this topic...</a></div>
        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="/ContextHelp/Form">
    <xsl:apply-templates select="Control" />
  </xsl:template>

  <xsl:template match="//LanguageReference">
    <div class="control">
      <xsl:attribute name="id">language/reference/<xsl:value-of select="@Name"/></xsl:attribute>
      <div class="title"><xsl:value-of select="@Title"/></div>
      <xsl:copy-of select="." />
      <div class="morehelp"><a>
        <xsl:attribute name='href'>help:language/reference/<xsl:value-of select="@Name"/></xsl:attribute>
        More help...
      </a></div>
    </div>
  </xsl:template>

  <xsl:template match="//Control">
    <div class="control">
      <xsl:attribute name="id"><xsl:value-of select="../@Name"/>-<xsl:value-of select="@Name"/></xsl:attribute>
      <div class="title"><xsl:value-of select="@Title"/></div>
      <xsl:copy-of select="." />
      <div class="morehelp"><a><xsl:attribute name='href'>help:<xsl:choose>
          <xsl:when test="@TopicName"><xsl:value-of select="@TopicName"></xsl:value-of></xsl:when>
          <xsl:otherwise><xsl:value-of select='../@Name'/></xsl:otherwise>
        </xsl:choose></xsl:attribute>More help...</a></div>
    </div>
  </xsl:template>
</xsl:stylesheet>
