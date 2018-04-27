<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  
  <xsl:template name="page_welcome">
    <div class="page" id="Welcome">
      <div id="uppertext0" style="clear: all; display: block;" >
        <div class="headerimage">
          <p><img alt="Welcome">
              <xsl:attribute name="src"><xsl:value-of select='/KeymanDeveloperProject/templatepath'/>header_welcome.gif</xsl:attribute>
          </img></p>
          <div class='quicklinks'>
            <h3>Quick Links</h3>
          
            <ul>
              <li><a href="keyman:checkforupdates">Check for Updates</a></li>
              <li><a href="help:index">Help Contents</a></li>
              <li><a href="help:context/project">Project Help</a></li>
            </ul>
          </div>
        </div>
        <div class="pagetext">
          <h2>Welcome to Keyman Developer!</h2>
          
          <h3>Getting Started</h3>
          
          <p>Creating a keyboard solution with Keyman Developer is easier than ever before.  The tabs at the bottom of this Project window
          illustrate the steps to creating a complete solution.  Each page has help and a tutorial to assist you in learning the ins-and-outs
          of Keyman Developer.</p>
          
          <p class="follow">Click the Keyboards tab to learn how to create your first keyboard.</p>
          
          <h3>Using the Project</h3>
          
          <p>You will find the easiest way to manage the files associated with your solution is to add them to this project.  A project is simply
          a collection of related files that can be edited by Keyman Developer, such as keyboards and packages.</p>
          
          <p>Keyman Developer will remember the last project you had open when you start, and recent projects can be quickly opened through 
          the Project menu.  After you first give your project a name by clicking Project menu-Save Project, any additional changes will be
          automatically saved.</p>
          
          <p>Recently edited files are associated with each project.  This page lists all the files you have recently edited while working
          with this project, sorted by last edit date.</p>
          
          <p>Deselecting the Show Help checkbox at the top right of the Project window will hide this help text once you don't need it any more.</p>
          
        </div>
      </div>

      <div style="padding: 0 10px 10px 10px;">
        <div style="clear:both">
          <h2>Recent Files</h2>
        </div>

        <div class='mrufilelist' id="mrulist">

          <div>
            <xsl:for-each select="KeymanDeveloperProject/MRU/File">
              <xsl:call-template name="file">
                <xsl:with-param name="file_has_details">true</xsl:with-param>
                <xsl:with-param name="file_has_no_options">true</xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </div>
        </div>
      </div>
    </div>
  </xsl:template>
  
  <xsl:template mode="options_menu" match="/KeymanDeveloperProject/MRU/File" >
    <div class="menu">
      <xsl:attribute name="id">menu_options_<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:call-template name="menuitem">
        <xsl:with-param name="caption">Open</xsl:with-param>
        <xsl:with-param name="command">keyman:openfile?id=<xsl:value-of select="ID" /></xsl:with-param>
      </xsl:call-template>
      <xsl:if test="FileType='.kmn' or FileType='.kps' or FileType='.kpp'">
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">View Source</xsl:with-param>
          <xsl:with-param name="command">keyman:viewfilesource?id=<xsl:value-of select="ID" /></xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="menuitem">
          <xsl:with-param name="caption">Open in External Editor</xsl:with-param>
          <xsl:with-param name="command">keyman:editfileexternal?id=<xsl:value-of select="ID" /></xsl:with-param>
        </xsl:call-template>
      </xsl:if>
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

  <xsl:template mode="filedetails" match="/KeymanDeveloperProject/MRU/File">
    <table class="filedetailtext">
      <tr>
        <th>Full path:</th><td><xsl:value-of select="FullPath"/></td>
      </tr>
    </table>
  </xsl:template>
</xsl:stylesheet>