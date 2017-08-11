<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">
  <xsl:output method="xml" version="1.0" encoding="utf-8" doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd" />

  <xsl:include href="elements.xsl"/>
  
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:k="http://www.tavultesoft.com/xml/70">
      <xsl:call-template name="head" />
      <body onload="javascript:pageload();">

        <div id="pageheader">
          Create a keyboard solution
        </div>
        <div id="pages" class="pages">
          <xsl:call-template name="page_keyboard" />
          <xsl:call-template name="page_packaging" />
          <xsl:call-template name="page_branding" />
          <xsl:call-template name="page_distribution" />
        </div>
        <div id="tabs" class="tabbackground">
          <div class="tabbspacer"></div>

          <div id="tabb1" class="tabb tabbselected" onmousedown="javascript:selecttabb(1);">
            <img src="tab_keyboard.gif" /> Keyboards
          </div>

          <div id="tabb2" class="tabb" onmousedown="javascript:selecttabb(2);">
            <img src="tab_package.gif" /> Packaging
          </div>

          <div id="tabb3" class="tabb" onmousedown="javascript:selecttabb(3);">
            <img src="tab_brand.gif" /> Branding
          </div>

          <div id="tabb4" class="tabb" onmousedown="javascript:selecttabb(4);">
            <img src="tab_distrib.gif" /> Distribution
          </div>
        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="page_keyboard">
    <!-- KEYBOARDS -->
    <div id="upperexpand" class="checkbox checked" onmousedown="javascript:showhideupper();">Show Keyboards help</div>
    <div id="Keyboards">

      <div id="uppertext1" style="clear: all; display: block;" >
        <div class="headerimage">
          <img src="header_keyboard.gif" /><br />Keyboards
        </div>
        <div class="pagetext">

          There are two ways to create a keyboard from scratch:<br /><br />
          <ul>
            <li>
              The Keyboard Wizard lets you quickly create a keyboard using a visual representation
              of a computer keyboard. You can drag and drop characters from a character map to create
              ANSI and Unicode keyboard layouts. To learn more about the Keyboard Wizard, click <a href="asd">here.</a>
            </li>
            <li>
              The Keyman Keyboard Language is used to create keyboards with complex character management
              such as constraints, dead keys, post-entry parsing and other powerful Keyman features.
              You can learn more about the Keyman Keyboard Language <a href="asd">here.</a>
            </li>
          </ul>
          If you have never created a keyboard before, you may want to work through the <a href="asd">Keyman Developer Tutorial.</a><br />

          <br />

          <div style="display: none;">
            <div class="iobox" style="width: 200px;">
              INPUTS
              <div class="ioinputbox">
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.KMN
                </span>
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.KVK
                </span>
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.BMP
                </span>
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.???
                </span>
              </div>
            </div>

            <div class="ioarrow"></div>

            <div class="iobox" style="width: 150px;">
              OUTPUTS
              <div class="iooutputbox">
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.KMX
                </span>
                <span class="ioicon">
                  <img src="kvk.gif" /><br />.KVK
                </span>
              </div>
            </div>
          </div>

        </div>
      </div>

      <div id="keyboardlist">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">New keyboard</xsl:with-param>
          <xsl:with-param name="command">keyman:newkeyboard</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Add existing keyboard</xsl:with-param>
          <xsl:with-param name="command">keyman:addexistingkeyboard</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Compile all</xsl:with-param>
          <xsl:with-param name="command">keyman:compileall</xsl:with-param>
        </xsl:call-template>

        <br />
        <br />

        <xsl:for-each select="KeymanDeveloperProject/Keyboards/Keyboard">
          <xsl:call-template name="keyboard" />
        </xsl:for-each>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="keyboard">
    <span tabindex="1" class="keyboarditem" onmousedown="javascript:this.focus(); globalfocus=this.id;">
      <xsl:attribute name="id">keyboarditem<xsl:value-of select="position()"/></xsl:attribute>
      <xsl:attribute name="onfocus">javascript:highlightkeyboard(<xsl:value-of select="position()"/>); globalfocus=this.id;</xsl:attribute>
      <xsl:attribute name="onblur">javascript:unhighlightkeyboard(<xsl:value-of select="position()"/>);</xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:hoverkeyboard(<xsl:value-of select="position()"/>);</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:unhoverkeyboard(<xsl:value-of select="position()"/>);</xsl:attribute>
      <xsl:attribute name="onkeypress">javascript:expanddetails(<xsl:value-of select="position()"/>);</xsl:attribute>

      <div class="keyboardexpand">
        <xsl:attribute name="id">keyboardplus<xsl:value-of select="position()"/></xsl:attribute>
        <xsl:attribute name="onmousedown">javascript:showdetails(<xsl:value-of select="position()"/>);</xsl:attribute>
        <xsl:text> </xsl:text>
      </div>

      <div class="keyboardicon">
        <img src="icon_kmn.gif" />
      </div>
      <div class="keyboardtext"><xsl:value-of select="Filename" /></div>
      <div class="keyboardname">(<xsl:value-of select="Name" />)</div>

      <div class="keyboardoptions">
        <xsl:call-template name="button">
          <xsl:with-param name="caption">Options</xsl:with-param>
          <xsl:with-param name="command">keyman:keyboardoptions</xsl:with-param>
        </xsl:call-template>
      </div>
      <div class="keyboarddetails">
        <xsl:attribute name="id">kbddetails<xsl:value-of select="position()"/></xsl:attribute>
        <div class="keyboarddetailtext">
          <img style="vertical-align: middle">
            <xsl:attribute name="src">
              <xsl:value-of select="/KeymanDeveloperProject/ImagePath"/>
              <xsl:value-of select="BitmapImage"/>
            </xsl:attribute>
          </img>
          <xsl:attribute name="onfocus">javascript:highlightkeyboard(<xsl:value-of select="position()"/>);</xsl:attribute>
          <xsl:attribute name="onblur">javascript:unhighlightkeyboard(<xsl:value-of select="position()"/>);</xsl:attribute>
          <xsl:text> </xsl:text><span tabindex="1">
            <xsl:attribute name="id">kbdbitmap<xsl:value-of select="position()"/></xsl:attribute>
            <xsl:value-of select="Bitmap" />
          </span><br />
          <table class="keyboarddetailtext">
            <tr>
              <th>Copyright:</th><td><xsl:value-of select="Copyright"/></td>
            </tr>
            <tr>
              <th>Message:</th><td><xsl:value-of select="Message"/></td>
            </tr>
          </table>
        </div>
      </div>
    </span>
  </xsl:template>

  <xsl:template name="page_packaging">
    <div id="Packaging" style="display: none;">
      <div id="uppertext2" style="clear: all; display: block;">
        <div class="headerimage">
          <img src="header_package.gif" /><br />Packaging
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="page_branding">
    <div id="Branding" style="display: none;">
      <div id="uppertext3" style="clear: all; display: block;">
        <div class="headerimage">
          <img src="header_branding.gif" /><br />Branding
        </div>
      </div>
    </div>
  </xsl:template>
  
  <xsl:template name="page_distribution">
    <div id="Distribution" style="display: none;">
      <div id="uppertext4" style="clear: all; display: block;">
        <div class="headerimage">
          <img src="header_distrib.gif" /><br />Distribution
        </div>
      </div>
    </div>
  </xsl:template>
  
  </xsl:stylesheet>