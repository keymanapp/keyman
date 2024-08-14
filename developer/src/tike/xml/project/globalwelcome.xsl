<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  <xsl:import href="elements.xsl"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:k="http://www.tavultesoft.com/xml/70">
      <head>
        <link rel='stylesheet' type='text/css' href='res/project.css' />
        <script src='res/common.js'><xsl:text> </xsl:text></script>
      </head>
      <body onload="pageload()" id='globalwelcome'>
        <div id="menubackground" onmousedown="HideMenu()"></div>
        <div id="pageheader">
            <div class='pagetext'>
              <p><img src='res/keymandeveloper.png' alt='Keyman Developer' /></p>
              <p id='header-link'><a><xsl:attribute name="href"><xsl:value-of select='/Data/DeveloperUrls/Url[@id="home"]/@href' /></xsl:attribute><xsl:value-of select='/Data/DeveloperUrls/Url[@id="home-presentation"]/@href' /></a></p>
              <p id='header-version'>Version <xsl:value-of select="/Data/Version" /></p>
              <div id='header-sil'>
                <img src='res/sil.png' />
                <div>Created by SIL Global
                  <div id='header-copyright'>Copyright &#169; SIL Global. All Rights Reserved.</div>
                </div>
              </div>

            </div>
        </div>
        <div class="pages" id="pages">
          <div class="help-container" id="uppertext0" style="display:block">
            <div style='clear:both'>
            </div>

            <div class="headerimage"></div>
            <div class="pagetext">
              <h1>Get Started!</h1>

              <p>Creating a keyboard or predictive text solution with Keyman Developer is easy! To get started, create or open a project.</p>

              <p>Keyman Developer includes two kinds of projects:</p>

              <ul>
                <li>Keyboard projects: Create a keyboard layout for any platform</li>
                <li>Lexical model projects: Create a predictive text model for mobile devices</li>
              </ul>

              <p>The <b>New Project</b> wizard will guide you through creating a <b>Basic</b> keyboard or a <b>Wordlist Lexical Model</b>; these are the
              recommended project types for new users.</p>

              <br/>

              <xsl:call-template name="button">
                <xsl:with-param name="caption">New Project...</xsl:with-param>
                <xsl:with-param name="command">keyman:newproject</xsl:with-param>
              </xsl:call-template>
              <xsl:text> </xsl:text>
              <xsl:call-template name="button">
                <xsl:with-param name="caption">Open Existing Project...</xsl:with-param>
                <xsl:with-param name="command">keyman:openproject</xsl:with-param>
              </xsl:call-template>
            </div>
          </div>

          <div style="padding: 0 10px 10px 10px;">
            <div style="clear:both">
              <h2>Recent Projects</h2>
            </div>

            <div class='mrufilelist' id="mrulist">

              <div>
                <xsl:for-each select="/Data/MRU/File">
                  <xsl:call-template name="file">
                    <xsl:with-param name="file_has_details">true</xsl:with-param>
                    <xsl:with-param name="file_has_no_options">true</xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </div>
            </div>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template mode="options_menu" match="/Data/MRU/File" >
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open</xsl:with-param>
        <xsl:with-param name="command">keyman:openproject?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Containing Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:opencontainingfolder?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Remove from list</xsl:with-param>
        <xsl:with-param name="command">keyman:removefrommru?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>

  <xsl:template mode="filedetails" match="/Data/MRU/File">
    <table class="filedetailtext">
      <tr>
        <th>Full path:</th><td><xsl:value-of select="FullPath"/></td>
      </tr>
    </table>
  </xsl:template>
</xsl:stylesheet>