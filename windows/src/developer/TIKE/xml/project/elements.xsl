<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

<xsl:template name="head">
  <head>
    <link rel='stylesheet' type='text/css' href='res/project.css' />
    <script src='res/project.js'><xsl:text> </xsl:text></script>
  </head>

</xsl:template>
  
<xsl:template name="checkbox">
  <input type="checkbox" />
</xsl:template>

<xsl:template name="button">
  <xsl:param name="caption" />
  <xsl:param name="command" />
  <xsl:param name="id" />
  <xsl:param name="notabstop" />
  <xsl:param name="enabled" />
  <xsl:param name="width" />
  
  <input type="button">
    <xsl:if test="not($notabstop)"><xsl:attribute name="tabindex">1</xsl:attribute></xsl:if>
    <xsl:attribute name="ID">button_<xsl:value-of select="$id"/>
    </xsl:attribute>
    <xsl:attribute name="onclick">location.href="<xsl:value-of select="$command"/>"</xsl:attribute>
    <xsl:if test="$enabled = 'false'"><xsl:attribute name="disabled">true</xsl:attribute></xsl:if>
    <xsl:attribute name="value">
      <xsl:value-of select="$caption" />
    </xsl:attribute>
    <xsl:attribute name="style">width: <xsl:value-of select="$width"/>; font: 8pt microsoft sans serif; height: 23px;</xsl:attribute>
  </input>
</xsl:template>

<!--
-
- File template
-
-->
  <xsl:template name="fileicon">
    <xsl:param name="FileType" />
    <img>
      <xsl:attribute name="alt"><xsl:value-of select="$FileType" /></xsl:attribute>
      <xsl:attribute name="src">res/icon_<xsl:choose>
          <xsl:when test='contains(".kmn.kmp.kmx.kps.kvk", $FileType)'><xsl:value-of select="substring($FileType,2)" />.png</xsl:when>
          <xsl:otherwise>file.gif</xsl:otherwise>
        </xsl:choose></xsl:attribute>
    </img>
  </xsl:template>
  
  <xsl:template name="file">
    <xsl:param name="file_description" />
    <xsl:param name="file_has_details" />
    <xsl:param name="file_has_no_options" />
    
    <span tabindex="1" class="file" onmousedown="javascript:this.focus();">
      <xsl:attribute name="id">file<xsl:value-of select="ID"/></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:hoverfile('<xsl:value-of select="ID"/>');</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:unhoverfile('<xsl:value-of select="ID"/>');</xsl:attribute>
      <xsl:attribute name="onkeydown">javascript:return file_keydown();</xsl:attribute>
      <xsl:attribute name="oncontextmenu">javascript:return file_mousedown();</xsl:attribute>
      
      <xsl:if test="$file_has_details = 'true'">
        <div class="fileexpand">
          <xsl:attribute name="id">fileplus<xsl:value-of select="ID"/></xsl:attribute>
          <xsl:attribute name="onmousedown">javascript:return showfiledetails('<xsl:value-of select="ID"/>');</xsl:attribute>
          <xsl:text> </xsl:text>
        </div>
      </xsl:if>
      <xsl:if test="not($file_has_details = 'true')">
        <div class="fileexpand_empty"></div>
      </xsl:if>

      <div class="fileicon">
        <xsl:call-template name="fileicon">
          <xsl:with-param name="FileType"><xsl:value-of select="FileType" /></xsl:with-param>
        </xsl:call-template>
      </div>
      <div class="filename">
        <a tabindex="-1">
          <xsl:attribute name="href">keyman:editfile?id=<xsl:value-of select="ID"/></xsl:attribute>
          <xsl:value-of select="Filename" />
        </a>
      </div>
      <div class="filedescription"><xsl:value-of select="$file_description"/></div>

      <xsl:if test="$file_has_no_options != 'true'">
        <div class="fileoptions">
          <xsl:call-template name="menubutton">
            <xsl:with-param name="caption">Options</xsl:with-param>
            <xsl:with-param name="notabstop">1</xsl:with-param>
            <xsl:with-param name="id">options_<xsl:value-of select="ID"/></xsl:with-param>
            <xsl:with-param name="align">right</xsl:with-param>
            <xsl:with-param name="className">grey</xsl:with-param>
          </xsl:call-template>
        </div>
      </xsl:if>
      
      <xsl:if test="$file_has_details = 'true'">
        <div class="filedetails">
          <xsl:attribute name="id">filedetails<xsl:value-of select="ID"/></xsl:attribute>
          <xsl:apply-templates mode="filedetails" select="." />
        </div>
      </xsl:if>

      <xsl:apply-templates mode="options_menu" select="." />
    </span>
  </xsl:template>
  
  <!-- 
  -
  - Popup menu implementation
  -
  -->

  <!-- xsl:template name="menu_style">
    /* Popup menu styles */
        .menu { position: absolute; display: block; background: white; visibility: hidden; border: solid 1px #ACA899; font: 8pt Tahoma; padding: 0px; background-image: url("menugrey.gif"); background-repeat: repeat-y; background-position: 0 0; }
        k\:menuitem { width: 100%; padding: 4 4 4 28; margin: 1px; }
        k\:menuitem.hover { color: #000000; background: #FFE3DB; border: solid 1px #DB704F; padding: 3 3 3 27; }
        k\:menuitem.down { background: #DB704F; }
  </xsl:template -->
  
  <xsl:template name="menuitem">
    <xsl:param name="caption" />
    <xsl:param name="command" />
    <k:menuitem>
      <xsl:attribute name="onmousedown">javascript:menuitemdown();</xsl:attribute>
      <xsl:attribute name="command">
        <xsl:value-of select="$command" />
      </xsl:attribute>
      <xsl:value-of select="$caption"/>
    </k:menuitem>
  </xsl:template>
  
  <xsl:template name="menubutton">
    <xsl:param name="caption" />
    <xsl:param name="id" />
    <xsl:param name="align" />
    <xsl:param name="className" />
    <xsl:param name="width" />
    
    <xsl:call-template name="button">
      <xsl:with-param name="caption"><xsl:value-of select="$caption" /></xsl:with-param>
      <xsl:with-param name="command">javascript:ShowMenu('<xsl:value-of select="$id"/>','<xsl:value-of select="$align" />');</xsl:with-param>
      <xsl:with-param name="id"><xsl:value-of select="$id"/></xsl:with-param>
      <xsl:with-param name="className"><xsl:value-of select="$className"/></xsl:with-param>
      <xsl:with-param name="width"><xsl:value-of select="$width"/></xsl:with-param>
    </xsl:call-template>
    
  </xsl:template>

  <!-- Filetype (Keyman) Templates -->
  
  <xsl:template name="filetype_kmn">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kmn</xsl:with-param> <xsl:with-param name="title">Keyman Keyboard Source File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_kmx">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kmx</xsl:with-param> <xsl:with-param name="title">Keyman Keyboard File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_kvks">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kvks</xsl:with-param> <xsl:with-param name="title">Keyman On Screen Keyboard Source File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_kvk">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kvk</xsl:with-param> <xsl:with-param name="title">Keyman On Screen Keyboard File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template name="filetype_kps">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kps</xsl:with-param> <xsl:with-param name="title">Keyman Package Source File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_kmp">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="icontype">png</xsl:with-param> <xsl:with-param name="type">kmp</xsl:with-param> <xsl:with-param name="title">Keyman Package File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Filetype (Non-Keyman) Templates -->

  <xsl:template name="filetype_msi">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="type">msi</xsl:with-param> <xsl:with-param name="title">Windows Installer File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_ttf">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="type">ttf</xsl:with-param> <xsl:with-param name="title">TrueType Font File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_ico">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="type">ico</xsl:with-param> <xsl:with-param name="title">Icon File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="filetype_html">
    <xsl:call-template name="filetype"> 
      <xsl:with-param name="type">html</xsl:with-param> <xsl:with-param name="title">HTML File</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Filetype Generic Templates -->

  <xsl:template name="filetype__arrow">
    <td class="ioarrow"><img src='res/icon32_arrow.gif' /></td>
  </xsl:template>
  
  <xsl:template name="filetype__plus">
    <td class="ioarrow"><img src='res/icon32_plus.gif' /></td>
  </xsl:template>    

  <xsl:template name="filetype">
    <xsl:param name="type" />
    <xsl:param name="icontype">gif</xsl:param>
    <xsl:param name="title" />
    <td><a>
      <xsl:attribute name="href">help:reference/file-types/<xsl:value-of select="$type" /></xsl:attribute>
      <xsl:attribute name="title"><xsl:value-of select="$title"/></xsl:attribute>
      <img>
        <xsl:attribute name="src">res/icon32_<xsl:value-of select="$type" />.<xsl:value-of select="$icontype" /></xsl:attribute>
      </img><br />.<xsl:value-of select="translate($type,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
    </a></td>
  </xsl:template>
  
</xsl:stylesheet>