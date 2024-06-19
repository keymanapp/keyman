<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="page_model">
    <div class="page" id="page-2">
      <div class="help-container" id="uppertext2">
        <div class="headerimage">
          <p>
            <img alt="Models" src='res/header_model.png' />
          </p>
          <div class='quicklinks'>
            <h3>Quick Links</h3>

            <ul>
              <li>
                <a href="help:guides/lexical-models/intro">Model Tutorial</a>
              </li>
            </ul>
          </div>
        </div>
        <div class="pagetext">
          <h2>Creating your first lexical model</h2>

          <p>
            Click the New Model button to create a lexical model for predictive text. Lexical model source files are TypeScript files.
            In version 12, the model format supported is "trie-1.0". The model source file will include references to wordlist source
            files of .tsv format, which can be edited in Keyman Developer or in a compatible spreadsheet program or generated from a
            number of other tools; check the <a href='help:guides/lexical-models/intro'>online documentation</a> for more details.
          </p>

          <p>
            Lexical model filenames should have the format <b><i>author</i>.<i>bcp47</i>.<i>uniq</i>.model.ts</b>. A compiled lexical
            model file is a JavaScript file.
          </p>

          <p>
            A corresponding package file will be needed (with extension <b>.model.kps</b>) in order to deploy the model to mobile
            devices. This can be created in the <b>Packages</b> tab. Note that a package file can contain either model file(s) or
            keyboard file(s), not both.
          </p>

          <table class="io">
            <tr>
              <th colspan="2">Source Files</th>
              <th>&#160;</th>
              <th colspan="1">Outputs</th>
            </tr>
            <tr>
              <xsl:call-template name="filetype_model_ts" />
              <xsl:call-template name="filetype_model_tsv" />
              <xsl:call-template name="filetype__arrow" />
              <xsl:call-template name="filetype_model_js" />
            </tr>
          </table>
        </div>
      </div>

      <xsl:call-template name="upgrade-warning" />

      <div class='filelist' id="modellist">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">New model...</xsl:with-param>
          <xsl:with-param name="command">keyman:fileaddnew?type=model</xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="KeymanDeveloperProject/Options/Version != '2.0' or not(KeymanDeveloperProject/Options/Version)">
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Add existing model...</xsl:with-param>
            <xsl:with-param name="command">keyman:fileaddexisting?type=model</xsl:with-param>
            <xsl:with-param name="width">auto</xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        |
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Build all</xsl:with-param>
          <xsl:with-param name="command">keyman:compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.ts' or FileType='.kps') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean all</xsl:with-param>
          <xsl:with-param name="command">keyman:cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kps' or FileType='.ts' or FileType='.kps') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        |
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Build models</xsl:with-param>
          <xsl:with-param name="command">keyman:model_compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[FileType='.ts' and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean models</xsl:with-param>
          <xsl:with-param name="command">keyman:model_cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[FileType='.ts' and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <br />
        <br />

        <div>
          <xsl:for-each select="$SourceModelFiles">
            <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
            <xsl:call-template name="file">
              <xsl:with-param name="file_description">
                <xsl:if test="string-length(Details/Name) &gt; 0">
                  (<xsl:value-of select="Details/Name" />)
                </xsl:if>
              </xsl:with-param>
              <xsl:with-param name="file_has_details">false</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/Files/File[FileType='.ts']" >
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open</xsl:with-param>
        <xsl:with-param name="command">keyman:openfile?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Build</xsl:with-param>
        <xsl:with-param name="command">keyman:compilefile?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Clean</xsl:with-param>
        <xsl:with-param name="command">keyman:cleanfile?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">View Source</xsl:with-param>
        <xsl:with-param name="command">keyman:viewfilesource?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open in External Editor</xsl:with-param>
        <xsl:with-param name="command">keyman:editfileexternal?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Source Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:opencontainingfolder?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Build Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:openbuildfolder?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:if test="KeymanDeveloperProject/Options/Version != '2.0' or not(KeymanDeveloperProject/Options/Version)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Remove from Project</xsl:with-param>
          <xsl:with-param name="command">keyman:removefile?id=<xsl:value-of select="ID" />
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/Files/File[FileType='.tsv']" >
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open</xsl:with-param>
        <xsl:with-param name="command">keyman:openfile?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">View Source</xsl:with-param>
        <xsl:with-param name="command">keyman:viewfilesource?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open in External Editor</xsl:with-param>
        <xsl:with-param name="command">keyman:editfileexternal?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open Containing Folder</xsl:with-param>
        <xsl:with-param name="command">keyman:opencontainingfolder?id=<xsl:value-of select="ID" />
        </xsl:with-param>
      </xsl:call-template>
      <xsl:if test="/KeymanDeveloperProject/Options/Version != '2.0' or not(/KeymanDeveloperProject/Options/Version)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Remove from Project</xsl:with-param>
          <xsl:with-param name="command">keyman:removefile?id=<xsl:value-of select="ID" />
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>
</xsl:stylesheet>