<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="page_packaging">
    <div class="page" id="page-3">
      <div class="help-container" id="uppertext3">
        <div class="headerimage">
          <p><img src='res/header_package.png' /></p>
          <div class='quicklinks'>
            <h3>Quick Links</h3>

            <ul>
              <li><a href="help:guides/distribute/tutorial">Package Tutorial</a></li>
              <li><a href="help:context/package-editor">Package Editor Help</a></li>
            </ul>
          </div>
        </div>

        <div class="pagetext">
          <h2>Package keyboards with fonts and documentation</h2>

          <p>After creating a keyboard, you should create a package in order to distribute the keyboard to all Windows, macOS, iOS and Android. A package can include multiple
          keyboards, fonts, a welcome page, documentation and shortcuts.</p>

          <p>A package can be installed into any of the end user Keyman apps, and uploaded to the Keyman website to share with others.</p>

          <p>Learn more about distributing your packages and keyboards on the Distribution tab.</p>

          <table class="io">
            <tr>
              <th colspan="4">Input Files</th>
              <th>&#160;</th>
              <th colspan="1">Package Source</th>
              <th>&#160;</th>
              <th colspan="1">Output</th>
            </tr>
            <tr>
              <xsl:call-template name="filetype_kmx" />
              <xsl:call-template name="filetype_kvk" />
              <xsl:call-template name="filetype_ttf" />
              <xsl:call-template name="filetype_html" />
              <xsl:call-template name="filetype__plus" />
              <xsl:call-template name="filetype_kps" />
              <xsl:call-template name="filetype__arrow" />
              <xsl:call-template name="filetype_kmp" />
            </tr>
          </table>
        </div>
      </div>

      <xsl:call-template name="upgrade-warning" />

      <div class='filelist' id="packagelist">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">New package...</xsl:with-param>
          <xsl:with-param name="command">keyman:fileaddnew?type=package</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="KeymanDeveloperProject/Options/Version != '2.0' or not(KeymanDeveloperProject/Options/Version)">
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Add existing package...</xsl:with-param>
            <xsl:with-param name="command">keyman:fileaddexisting?type=package</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        |
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Build all</xsl:with-param>
          <xsl:with-param name="command">keyman:compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.kmn' or FileType='.xml-ldml-keyboard') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean all</xsl:with-param>
          <xsl:with-param name="command">keyman:cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.kmn' or FileType='.xml-ldml-keyboard') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        |
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Build packages</xsl:with-param>
          <xsl:with-param name="command">keyman:package_compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[FileType='.kps' and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean packages</xsl:with-param>
          <xsl:with-param name="command">keyman:package_cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[FileType='.kps' and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>

        <br />
        <br />

        <div>
          <xsl:for-each select="$SourcePackageFiles">
            <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
            <xsl:call-template name="file">
              <xsl:with-param name="file_description"><xsl:if test="string-length(Details/Name) &gt; 0">(<xsl:value-of select="Details/Name" />)</xsl:if></xsl:with-param>
              <xsl:with-param name="file_has_details">
                <xsl:if test="$FileState/FullPath or (Details/Copyright) or (Details/Version) or (/KeymanDeveloperProject/Files/File[ParentFileID=current()/ID])">true</xsl:if>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </div>
      </div>

    </div>
  </xsl:template>

  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/Files/File[FileType='.kps']" >
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
        <xsl:with-param name="caption">Open Source Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:opencontainingfolder?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Build Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:openbuildfolder?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:if test="/KeymanDeveloperProject/Options/Version != '2.0' or not(/KeymanDeveloperProject/Options/Version)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Remove from Project</xsl:with-param>
          <xsl:with-param name="command">keyman:removefile?id=<xsl:value-of select="ID" /></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template mode="filedetails" match="/KeymanDeveloperProject/Files/File[FileType='.kps']">
    <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
    <table class="filedetailtext">
      <xsl:if test="$FileState/FullPath">
        <tr>
          <th>Full path:</th><td><xsl:value-of select="$FileState/FullPath"/></td>
        </tr>
      </xsl:if>
      <xsl:if test="Details/Copyright">
        <tr>
          <th>Copyright:</th><td><xsl:value-of select="Details/Copyright"/></td>
        </tr>
      </xsl:if>
      <xsl:if test="Details/Version">
        <tr>
          <th>Version:</th><td><xsl:value-of select="Details/Version"/></td>
        </tr>
      </xsl:if>
      <xsl:if test="/KeymanDeveloperProject/Files/File[ParentFileID=current()/ID]">
        <tr>
          <th>Files:</th>
          <td>
            <xsl:apply-templates mode="packagefiles" select="/KeymanDeveloperProject/Files/File[ParentFileID=current()/ID]" />
          </td>
        </tr>
      </xsl:if>
    </table>
  </xsl:template>

  <xsl:template mode="packagefiles" match="/KeymanDeveloperProject/Files/File">
    <xsl:call-template name="fileicon">
      <xsl:with-param name="FileType"><xsl:value-of select="FileType" /></xsl:with-param>
    </xsl:call-template>
    <xsl:text> </xsl:text><span tabindex="1">
      <xsl:attribute name="id">packagefile<xsl:value-of select="ID"/></xsl:attribute>
      <a>
        <xsl:attribute name="href">keyman:editfile?id=<xsl:value-of select="ID"/></xsl:attribute>
        <xsl:value-of select="Filename" />
      </a>
    </span><br />
  </xsl:template>

</xsl:stylesheet>