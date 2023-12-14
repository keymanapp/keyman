<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="page_distribution">
    <div class="page" id="page-4">
      <div class="help-container" id="uppertext4">
        <div class="headerimage">
          <p><img alt="Distribution" src='res/header_distrib.png' /></p>
          <div class='quicklinks'>
            <h3>Quick Links</h3>

            <ul>
              <li><a><xsl:attribute name="href"><xsl:value-of select='KeymanDeveloperProject/DeveloperUrls/Url[@id="help-keyboards"]/@href' /></xsl:attribute>Share source and add to Keyman keyboard repositories</a></li>
              <li><a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-packages"]/@href' /></xsl:attribute>Keyboard distribution</a></li>
              <li><a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-mobile"]/@href' /></xsl:attribute>Distribution tips for mobile devices</a></li>
              <li><a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="keymanweb"]/@href' /></xsl:attribute>Web soft keyboard</a></li>
              <li><a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="keyman-engine-home"]/@href' /></xsl:attribute>Keyman Engine</a></li>
            </ul>
          </div>
        </div>

        <div class="pagetext">

          <h2>Distribute Your Keyboard Layouts</h2>

          <p>After completing your keyboards and packages, share them with the world.</p>

          <p>Keyman keyboards can be shared in several different ways:</p>

          <ul>
            <li>Add your keyboard to the Keyman Keyboard Repositories. In order to do this, you must share the source code of your keyboard.
                <b>Recommended option!</b> &#160; <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-keyboards"]/@href' /></xsl:attribute>Learn more</a></li>
            <li>Share a package file yourself: just give a .kmp file to any Keyman user, on Windows, macOS, iOS, Android or Linux and they can install it. Or put the .kmp file onto a website for users to download.
                <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-packages"]/@href' /></xsl:attribute>Learn more</a></li>
            <li>Sharing package files to mobile devices can be more complex. Learn about specific techniques for distribution to mobile devices.
                <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-mobile"]/@href' /></xsl:attribute>Learn more</a></li>
            <li>Add your keyboard to a website using KeymanWeb
                <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="keymanweb"]/@href' /></xsl:attribute>Learn more</a></li>
          </ul>

          <p>You can also create your own custom keyboarding product based on Keyman: <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="keyman-engine-home"]/@href' /></xsl:attribute>Learn more</a></p>

        </div>
      </div>

      <xsl:call-template name="upgrade-warning" />

      <div class='filelist' id="distributionlist">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">New file...</xsl:with-param>
          <xsl:with-param name="command">keyman:fileaddnew?type=text</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="KeymanDeveloperProject/Options/Version != '2.0' or not(KeymanDeveloperProject/Options/Version)">
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Add existing file...</xsl:with-param>
            <xsl:with-param name="command">keyman:fileaddexisting?type=text</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        |
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Build all</xsl:with-param>
          <xsl:with-param name="command">keyman:compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.kmn' or FileType='.xml-ldml-keyboard' or FileType='.ts' or FileType='.tsv') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean all</xsl:with-param>
          <xsl:with-param name="command">keyman:cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.kmn' or FileType='.xml-ldml-keyboard' or FileType='.ts' or FileType='.tsv') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>

        <br />
        <br />

        <div>
          <xsl:for-each select="$NonSourceFiles">
            <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
            <xsl:call-template name="file">
              <xsl:with-param name="file_description"></xsl:with-param>
              <xsl:with-param name="file_has_details">false</xsl:with-param>
              <xsl:with-param name="file_relative_path">true</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/Files/File[FileType!='.kps' and FileType!='.kmn' and FileType!='.xml-ldml-keyboard' and FileType!='.ts' and FileType!='.tsv']" >
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open</xsl:with-param>
        <xsl:with-param name="command">keyman:openfile?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Build</xsl:with-param>
        <xsl:with-param name="command">keyman:compilefile?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Clean</xsl:with-param>
        <xsl:with-param name="command">keyman:cleanfile?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">View Source</xsl:with-param>
        <xsl:with-param name="command">keyman:viewfilesource?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open in External Editor</xsl:with-param>
        <xsl:with-param name="command">keyman:editfileexternal?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Containing Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:opencontainingfolder?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:if test="/KeymanDeveloperProject/Options/Version != '2.0' or not(/KeymanDeveloperProject/Options/Version)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Remove from Project</xsl:with-param>
          <xsl:with-param name="command">keyman:removefile?id=<xsl:value-of select="ID" /></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

</xsl:stylesheet>
