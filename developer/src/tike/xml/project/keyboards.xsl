<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="page_keyboard">
    <xsl:variable name="isLdmlKeyboard"><xsl:if test="/KeymanDeveloperProject/Files/File[FileType='.xml-ldml-keyboard']">1</xsl:if></xsl:variable>
    <xsl:variable name="isKmnKeyboard"><xsl:if test="/KeymanDeveloperProject/Files/File[FileType='.kmn']">1</xsl:if></xsl:variable>
    <div class="page" id="page-1">
      <div class="help-container" id="uppertext1">
        <div class="headerimage">
          <p>
            <img alt="Keyboards" src='res/header_keyboard.png' />
          </p>
          <div class='quicklinks'>
            <h3>Quick Links</h3>

            <ul>
              <xsl:if test="$isLdmlKeyboard = 1">
                <!-- TODO: add links for LDML Keyboard Development -->
              </xsl:if>
              <xsl:if test="$isKmnKeyboard = 1">
                <li>
                  <a href="help:guides/develop/tutorial">Keyboard Tutorial</a>
                </li>
                <li>
                  <a href="help:language/">Keyman Keyboard Language Reference</a>
                </li>
              </xsl:if>
            </ul>
          </div>
        </div>
        <div class="pagetext">
          <h2>Creating your first keyboard</h2>

          <xsl:if test="$isLdmlKeyboard = 1">
            <p>
              Double-click your keyboard file in the list below to open it in the LDML Keyboard Editor. In Keyman Developer 17, this
              is a basic XML editor with an integrated test pane, available from the Keyboard menu.
            </p>
          </xsl:if>

          <xsl:if test="$isKmnKeyboard = 1">
            <p>
              Double-click your keyboard file in the list below to open it in the Keyboard Editor, where you can
              create a keyboard visually, or programmatically with the Keyman Keyboard Language.
            </p>

            <ul>
              <li>
                The <a href="help:context/keyboard-editor#toc-layout-tab">Layout page</a> in the Keyboard Editor lets you quickly create
                a keyboard using a visual representation of a computer keyboard. You can drag and drop characters from the
                <a href="help:context/character-map">Character Map</a> to create Unicode keyboard layouts.
              </li>
              <li>
                The <a href="help:context/keyboard-editor#toc-layout-tab">Source tab</a> of the layout page shows the keyboard's design in the
                <a href="help:reference">Keyman Keyboard Language</a>.  From here, you can enhance keyboards with input management features
                such as constraints, dead keys, character reordering, and more.  Read the <a href="help:guides/develop/tutorial">Tutorial</a>
                for an introduction to these features.
              </li>
            </ul>
          </xsl:if>

          <p>
            It's a good idea to read <a><xsl:attribute name="href"><xsl:value-of select='/KeymanDeveloperProject/DeveloperUrls/Url[@id="help-keyboards"]/@href' /></xsl:attribute>Developing Open Source Keyboards</a> for guidelines
            on preparing open source keyboards for sharing through the Keyman keyboard repositories. Also see the Distribution tab for more on
            distributing your completed keyboards.
          </p>

          <table class="io">
            <tr>
              <xsl:if test="$isLdmlKeyboard = 1">
                <th colspan="1">Source Files</th>
              </xsl:if>
              <xsl:if test="$isKmnKeyboard = 1">
                <th colspan="3">Source Files</th>
              </xsl:if>
              <th>&#160;</th>
              <th colspan="2">Outputs</th>
            </tr>
            <tr>
              <xsl:if test="$isLdmlKeyboard = 1">
                <xsl:call-template name="filetype_xml" />
              </xsl:if>
              <xsl:if test="$isKmnKeyboard = 1">
                <xsl:call-template name="filetype_kmn" />
                <xsl:call-template name="filetype_kvks" />
                <xsl:call-template name="filetype_ico" />
              </xsl:if>
              <xsl:call-template name="filetype__arrow" />
              <xsl:call-template name="filetype_kmx" />
              <xsl:call-template name="filetype_kvk" />
            </tr>
          </table>
        </div>
      </div>

      <xsl:call-template name="upgrade-warning" />

      <div class='filelist' id="keyboardlist">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">New keyboard...</xsl:with-param>
          <xsl:with-param name="command">keyman:fileaddnew?type=keyboard</xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="KeymanDeveloperProject/Options/Version != '2.0' or not(KeymanDeveloperProject/Options/Version)">
          <xsl:call-template name="button">
            <xsl:with-param name="caption">Add existing keyboard...</xsl:with-param>
            <xsl:with-param name="command">keyman:fileaddexisting?type=keyboard</xsl:with-param>
            <xsl:with-param name="width">auto</xsl:with-param>
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
          <xsl:with-param name="caption">Build keyboards</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_compileall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kmn' or FileType='.xml-ldml-keyboard') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Clean keyboards</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboard_cleanall</xsl:with-param>
          <xsl:with-param name="enabled">
            <xsl:if test="not(KeymanDeveloperProject/Files/File[(FileType='.kmn' or FileType='.xml-ldml-keyboard') and not (ParentFileID)])">false</xsl:if>
          </xsl:with-param>
          <xsl:with-param name="width">auto</xsl:with-param>
        </xsl:call-template>
        <br />
        <br />

        <div>
          <xsl:for-each select="$SourceKeyboardFiles">
            <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
            <xsl:call-template name="file">
              <xsl:with-param name="file_description">
                <xsl:if test="string-length(Details/Name) &gt; 0">
                  (<xsl:value-of select="Details/Name" />)
                </xsl:if>
              </xsl:with-param>
              <xsl:with-param name="file_has_details">
                <xsl:if test="$FileState/FullPath or (Details/Copyright) or (Details/Message) or (/KeymanDeveloperProject/Files/File[ParentFileID=current()/ID])">true</xsl:if>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/Files/File[FileType='.kmn' or FileType='.xml-ldml-keyboard']" >
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
      <xsl:if test="/KeymanDeveloperProject/Options/Version != '2.0' or not(/KeymanDeveloperProject/Options/Version)">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Remove from Project</xsl:with-param>
          <xsl:with-param name="command">keyman:removefile?id=<xsl:value-of select="ID" />
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template mode="filedetails" match="/KeymanDeveloperProject/Files/File[FileType='.kmn' or FileType='.xml-ldml-keyboard']">
    <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
    <table class="filedetailtext">
      <xsl:if test="$FileState/FullPath">
        <tr>
          <th>Full path:</th>
          <td>
            <xsl:value-of select="$FileState/FullPath"/>
          </td>
        </tr>
      </xsl:if>
      <xsl:apply-templates mode="keyboardbitmap" select="/KeymanDeveloperProject/Files/File[ParentFileID=current()/ID]" />
      <xsl:if test="Details/Copyright">
        <tr>
          <th>Copyright:</th>
          <td>
            <xsl:value-of select="Details/Copyright"/>
          </td>
        </tr>
      </xsl:if>
      <xsl:if test="FileVersion">
        <tr>
          <th>Version:</th>
          <td>
            <xsl:value-of select="FileVersion"/>
          </td>
        </tr>
      </xsl:if>
      <xsl:if test="Details/Message">
        <tr>
          <th>Message:</th>
          <td>
            <xsl:value-of select="Details/Message"/>
          </td>
        </tr>
      </xsl:if>
    </table>
  </xsl:template>

  <xsl:template mode="keyboardbitmap" match="/KeymanDeveloperProject/Files/File">
    <xsl:variable name="FileState" select="/KeymanDeveloperProject/FileStates/FileState[ID=current()/ID]" />
    <tr>
      <th>Bitmap:</th>
      <td>
        <img style="vertical-align: middle; width: 16px; height: 16px;">
          <xsl:attribute name="src"><xsl:value-of select="'ico?path='+$FileState/FullPath"/></xsl:attribute>
        </img>
        <xsl:text> </xsl:text>
        <span tabindex="1">
          <xsl:attribute name="id">kbdbitmap<xsl:value-of select="ID"/></xsl:attribute>
          <a>
            <xsl:attribute name="href">keyman:editfile?id=<xsl:value-of select="ID"/></xsl:attribute>
            <xsl:value-of select="Filename" />
          </a>
        </span>
        <br />
      </td>
    </tr>
  </xsl:template>
</xsl:stylesheet>