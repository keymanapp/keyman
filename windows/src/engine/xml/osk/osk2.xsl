<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE xsl:stylesheet[
  <!ENTITY keywidth "34">
  <!ENTITY keyheight "35">
  <!ENTITY keywidth_bksp "60">
  <!ENTITY keywidth_tab "52">
  <!ENTITY keywidth_caps "62">
  <!ENTITY keywidth_enter "70">
  <!ENTITY keywidth_shiftl "80">
  <!ENTITY keywidth_shiftr "90">
  <!ENTITY keywidth_ctrll "60">
  <!ENTITY keywidth_altl "50">
  <!ENTITY keywidth_altr "50">
  <!ENTITY keywidth_ctrlr "60">
  <!ENTITY keywidth_space "234">
  <!ENTITY keywidth_bkslash "42">
  <!ENTITY keywidth_gapl "30">
  <!ENTITY keywidth_gapr "54">
  <!ENTITY keycap "key">
  <!ENTITY textright "5">
  <!ENTITY textbottom "4">
]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:keys="http://www.tavultesoft.com/xml/keys" xmlns:oskexportdetails="http://www.tavultesoft.com/xml/oskexportdetails">

  <xsl:output encoding="utf-16" method="html" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" /><!--I2585-->
  <xsl:param name='folders' select='1' />
  <xsl:param name='graphical' select='1' />

  <!-- Lists files that will be copied into the _files\ subfolder by the HTML export --> 
  <oskexportdetails:oskexportdetails>
    <oskexportdetails:name>Graphical Keys</oskexportdetails:name>
    <oskexportdetails:includefiles>
      <oskexportdetails:includefile>key-altl.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-altr.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-backspace.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-bkslash.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-capslock.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-ctrll.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-ctrlr.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-enter.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-plain.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-shift102.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-shiftl.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-shiftr.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-space.gif</oskexportdetails:includefile>
      <oskexportdetails:includefile>key-tab.gif</oskexportdetails:includefile>
    </oskexportdetails:includefiles>
  </oskexportdetails:oskexportdetails>

  <xsl:variable name="imgpath"><xsl:if test='$folders'><xsl:value-of select="/visualkeyboard/header/filename"/>_files/</xsl:if></xsl:variable>
  <xsl:variable name="keycap"><xsl:if test="$graphical">&keycap;</xsl:if></xsl:variable>
  
  <xsl:template match="/">
    <xsl:variable name="root" select="/" />
    <html>
      <head>
        <xsl:call-template name="styles" />
      </head>
      <body>
        <xsl:for-each select="document('')//keys:unicode">
          <xsl:variable name="unicode" select="." />
          <xsl:for-each select="document('')//keys:shiftstate">
            <xsl:variable name="shift" select="." />
            <xsl:if test="$root/visualkeyboard/keys/key[@unicode=$unicode/@unicode and @shift=$shift/@shift]">
              <h1 style="clear:left"><xsl:value-of select="$unicode/@name"/> : <xsl:value-of select="@name"/></h1>
              <xsl:apply-templates mode="keyboard" select="document('')//keys:defkey">
                <xsl:with-param name="root" select="$root"/>
                <xsl:with-param name="shift" select="$shift"/>
                <xsl:with-param name="unicode" select="$unicode"/>
                <xsl:sort select="document('')//keys:defkey[@vkey=current()/@vkey]/../@index" data-type="number" />
                <xsl:sort select="document('')//keys:defkey[@vkey=current()/@vkey]/@index" data-type="number" />
              </xsl:apply-templates>
            </xsl:if>
          </xsl:for-each>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="styles">
    <style>
      <xsl:attribute name="type">text/css</xsl:attribute>
        .key { 
            float: left;
            display: block; 
            position: relative;
            overflow: hidden;
            height: &keyheight;px;
            <xsl:choose>
              <xsl:when test="$graphical = ''">
                margin: 2px 0px 0px 2px;
                border: solid 1px grey;
              </xsl:when>
              <xsl:otherwise>
                margin: 2px 0px 0px 4px;
              </xsl:otherwise>
            </xsl:choose>
        }
        
        #K_SPACE {
          width: &keywidth_space;px; 
          <xsl:if test="$graphical != ''">
            background-image: url('<xsl:value-of select="$imgpath" />&keycap;-space.gif'); 
          </xsl:if>
        }

        #K_BKSLASH {
          width: &keywidth_bkslash;px; 
          <xsl:if test="$graphical != ''">
            background-image: url('<xsl:value-of select="$imgpath" />&keycap;-bkslash.gif'); 
          </xsl:if>
        }
        
        #K_oE2 {
          <xsl:if test="not(/visualkeyboard/header/flags/key102)">
            display:none;
          </xsl:if>
        }
        
        #K_SHIFTL {
          <xsl:if test="/visualkeyboard/header/flags/key102">width: 42px;</xsl:if>
          <xsl:if test="not(/visualkeyboard/header/flags/key102)">width: &keywidth_shiftl;px;</xsl:if>
          <xsl:if test="$graphical != ''">
            <xsl:if test="/visualkeyboard/header/flags/key102">background-image: url('<xsl:value-of select="$imgpath" />&keycap;-shift102.gif');</xsl:if>
            <xsl:if test="not(/visualkeyboard/header/flags/key102)">background-image: url('<xsl:value-of select="$imgpath" />&keycap;-shiftl.gif');</xsl:if>
          </xsl:if>
        }
        
        .plain {
          <xsl:if test="$graphical != ''">
            background-image: url('<xsl:value-of select="$imgpath" />&keycap;-plain.gif');
          </xsl:if>
          background-repeat: no-repeat;
          width: &keywidth;px;
        }
        
        .special {
          background-repeat: no-repeat;
          <xsl:if test="$graphical = ''">
            background: #c0c0c0;
          </xsl:if>
        }
        
        .keycap {
          font: bold 7pt Arial; 
          position: absolute; 
          left: 6px; 
          top: 6px;
        }
        
        .key .keycap {          
          <xsl:choose>
            <xsl:when test="/visualkeyboard/header/flags/displayunderlying">display: block;</xsl:when>
            <xsl:otherwise>display: none;</xsl:otherwise>
          </xsl:choose>
        }
        
        .special .keycap {
          display: block;
        }
        
        .keytext {
          
					font:
						<xsl:choose><!-- I1185 -->
							<xsl:when test="/visualkeyboard/header/unicodefont/font/@size &lt; 0">
								<xsl:value-of select="-(/visualkeyboard/header/unicodefont/font/@size)" />pt
							</xsl:when>
							<xsl:otherwise>
								<xsl:value-of select="/visualkeyboard/header/unicodefont/font/@size" />pt
							</xsl:otherwise>
						</xsl:choose> 
						"<xsl:value-of select="/visualkeyboard/header/unicodefont/font/@name" />"; 
          position: absolute; 
          display: block;
          right: &textright;px;
          bottom: &textbottom;px;
          color: blue;
        }
    </style>
  </xsl:template>
  
  <keys:defkeys>
    <keys:defrow index="0">
      <keys:defkey index="0" cap="`" vkey="K_BKQUOTE" />
      <keys:defkey index="1" cap="1" vkey="K_1" />
      <keys:defkey index="2" cap="2" vkey="K_2" />
      <keys:defkey index="6" cap="3" vkey="K_3" />
      <keys:defkey index="7" cap="4" vkey="K_4" />
      <keys:defkey index="8" cap="5" vkey="K_5" />
      <keys:defkey index="9" cap="6" vkey="K_6" />
      <keys:defkey index="10" cap="7" vkey="K_7" />
      <keys:defkey index="11" cap="8" vkey="K_8" />
      <keys:defkey index="12" cap="9" vkey="K_9" />
      <keys:defkey index="13" cap="0" vkey="K_0" />
      <keys:defkey index="14" cap="-" vkey="K_HYPHEN" />
      <keys:defkey index="15" cap="=" vkey="K_EQUAL" />
    </keys:defrow>
    <keys:defrow index="1">
      <keys:defkey index="0" cap="Q" vkey="K_Q" />
      <keys:defkey index="1" cap="W" vkey="K_W" />
      <keys:defkey index="2" cap="E" vkey="K_E" />
      <keys:defkey index="3" cap="R" vkey="K_R" />
      <keys:defkey index="4" cap="T" vkey="K_T" />
      <keys:defkey index="5" cap="Y" vkey="K_Y" />
      <keys:defkey index="6" cap="U" vkey="K_U" />
      <keys:defkey index="7" cap="I" vkey="K_I" />
      <keys:defkey index="8" cap="O" vkey="K_O" />
      <keys:defkey index="9" cap="P" vkey="K_P" />
      <keys:defkey index="10" cap="[" vkey="K_LBRKT" />
      <keys:defkey index="11" cap="]" vkey="K_RBRKT" />
      <keys:defkey index="12" cap="\" vkey="K_BKSLASH" />
    </keys:defrow>
    <keys:defrow index="2">
      <keys:defkey index="0" cap="A" vkey="K_A" />
      <keys:defkey index="1" cap="S" vkey="K_S" />
      <keys:defkey index="2" cap="D" vkey="K_D" />
      <keys:defkey index="3" cap="F" vkey="K_F" />
      <keys:defkey index="4" cap="G" vkey="K_G" />
      <keys:defkey index="5" cap="H" vkey="K_H" />
      <keys:defkey index="6" cap="J" vkey="K_J" />
      <keys:defkey index="7" cap="K" vkey="K_K" />
      <keys:defkey index="8" cap="L" vkey="K_L" />
      <keys:defkey index="9" cap=";" vkey="K_COLON" />
      <keys:defkey index="10" cap="'" vkey="K_QUOTE" />
    </keys:defrow>
    <keys:defrow index="3">
      <keys:defkey index="0" cap="\" vkey="K_oE2" />
      <keys:defkey index="1" cap="Z" vkey="K_Z" />
      <keys:defkey index="2" cap="X" vkey="K_X" />
      <keys:defkey index="3" cap="C" vkey="K_C" />
      <keys:defkey index="4" cap="V" vkey="K_V" />
      <keys:defkey index="5" cap="B" vkey="K_B" />
      <keys:defkey index="6" cap="N" vkey="K_N" />
      <keys:defkey index="7" cap="M" vkey="K_M" />
      <keys:defkey index="8" cap="," vkey="K_COMMA" />
      <keys:defkey index="9" cap="." vkey="K_PERIOD" />
      <keys:defkey index="10" cap="/" vkey="K_SLASH" />
    </keys:defrow>
    <keys:defrow index="4">
      <keys:defkey index="10" cap=" " vkey="K_SPACE" />
    </keys:defrow>
  </keys:defkeys>
  
  <keys:unicode unicode="0" name="Codepage" />
  <keys:unicode unicode="1" name="Unicode" />
  <keys:shiftstate shift="" name="Unshifted" />
  <keys:shiftstate shift="S" name="Shift" />

  <keys:shiftstate shift="C" name="Control" />
  <keys:shiftstate shift="LC" name="Left Control" />
  <keys:shiftstate shift="RC" name="Right Control" />
  <keys:shiftstate shift="SC" name="Shift+Control" />
  <keys:shiftstate shift="SLC" name="Shift+Left Control" />
  <keys:shiftstate shift="SRC" name="Shift+Right Control" />
  
  <keys:shiftstate shift="A" name="Alt" />
  <keys:shiftstate shift="LA" name="Left Alt" />
  <keys:shiftstate shift="RA" name="Right Alt" />
  <keys:shiftstate shift="SA" name="Shift+Alt" />
  <keys:shiftstate shift="SLA" name="Shift+Left Alt" />
  <keys:shiftstate shift="SRA" name="Shift+Right Alt" />
  <keys:shiftstate shift="CA" name="Control+Alt" />
  <keys:shiftstate shift="LCLA" name="Left Control+Left Alt" />
  <keys:shiftstate shift="LCRA" name="Left Control+Right Alt" />
  <keys:shiftstate shift="RCLA" name="Right Control+Left Alt" />
  <keys:shiftstate shift="RCRA" name="Right Control+Right Alt" />

  <keys:shiftstate shift="SCA" name="Shift+Control+Alt" />
  <keys:shiftstate shift="SLCLA" name="Shift+Left Control+Left Alt" />
  <keys:shiftstate shift="SLCRA" name="Shift+Left Control+Right Alt" />
  <keys:shiftstate shift="SRCLA" name="Shift+Right Control+Left Alt" />
  <keys:shiftstate shift="SRCRA" name="Shift+Right Control+Right Alt" />
  
  <xsl:template name="specialkey">
    <xsl:param name="width" />
    <xsl:param name="cap" />
    <xsl:param name="id" />
    <xsl:param name="keycapimage" />
    <div class="key special">
      <xsl:attribute name="id"><xsl:value-of select="$id"/></xsl:attribute>
      <xsl:attribute name="style">
        width: <xsl:value-of select="$width"/>px;
        <xsl:if test="$graphical != ''">
          <xsl:if test="$keycapimage">
          background-image: url('<xsl:value-of select="$imgpath" />&keycap;-<xsl:value-of select="$keycapimage" />');
          </xsl:if>
        </xsl:if>
      </xsl:attribute>
      
      <div class="keycap"><xsl:value-of select="$cap" /></div>
    </div>
  </xsl:template>
  
  <xsl:template name="specialkeys">
    <xsl:param name="root" />
    <xsl:param name="index" />
    <xsl:choose>
      <xsl:when test="$index = 1">
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Backspace'"/>
          <xsl:with-param name="keycapimage" select="'backspace.gif'"/>
          <xsl:with-param name="width" select="&keywidth_bksp;" />
        </xsl:call-template>
        <br style="clear: left"/>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Tab'"/>
          <xsl:with-param name="keycapimage" select="'tab.gif'"/>
          <xsl:with-param name="width" select="&keywidth_tab;" />
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$index = 2">
        <br style="clear: left"/>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Caps Lock'"/>
          <xsl:with-param name="keycapimage" select="'capslock.gif'"/>
          <xsl:with-param name="width" select="&keywidth_caps;" />
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$index = 3">
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Enter'"/>
          <xsl:with-param name="keycapimage" select="'enter.gif'"/>
          <xsl:with-param name="width" select="&keywidth_enter;" />
        </xsl:call-template>
        <br style="clear: left"/>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Shift'"/>
          <xsl:with-param name="id" select="'K_SHIFTL'"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$index = 4">
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap" select="'Shift'"/>
          <xsl:with-param name="keycapimage" select="'shiftr.gif'"/>
          <xsl:with-param name="width" select="&keywidth_shiftr;" />
        </xsl:call-template>
        <br style="clear: left"/>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap">
            <xsl:choose>
              <xsl:when test="$root/visualkeyboard/header/flags/usealtgr">L Ctrl</xsl:when>
              <xsl:otherwise>Ctrl</xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="keycapimage" select="'ctrll.gif'"/>
          <xsl:with-param name="width" select="&keywidth_ctrll;" />
        </xsl:call-template>
        <div style="width: &keywidth_gapl;px; display: block; float: left;">&#160;</div>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap">
            <xsl:choose>
              <xsl:when test="$root/visualkeyboard/header/flags/usealtgr">L ALt</xsl:when>
              <xsl:otherwise>Alt</xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="keycapimage" select="'altl.gif'"/>
          <xsl:with-param name="width" select="&keywidth_altl;" />
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$index = 5">
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap">
            <xsl:choose>
              <xsl:when test="$root/visualkeyboard/header/flags/usealtgr">R ALt</xsl:when>
              <xsl:otherwise>Alt</xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="keycapimage" select="'altr.gif'"/>
          <xsl:with-param name="width" select="&keywidth_altr;" />
        </xsl:call-template>
        <div style="width: &keywidth_gapr;px; display: block; float: left;">&#160;</div>
        <xsl:call-template name="specialkey">
          <xsl:with-param name="cap">
            <xsl:choose>
              <xsl:when test="$root/visualkeyboard/header/flags/usealtgr">R Ctrl</xsl:when>
              <xsl:otherwise>Ctrl</xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="keycapimage" select="'ctrlr.gif'"/>
          <xsl:with-param name="width" select="&keywidth_ctrlr;"/>
        </xsl:call-template>
        <br style="clear: left"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="//keys:defkey" mode="keyboard">
    <xsl:param name="root" />
    <xsl:param name="shift" />
    <xsl:param name="unicode" />
    <xsl:if test="(parent::node()/keys:defkey[1]/@vkey)=current()/@vkey">
      <xsl:call-template name="specialkeys">
        <xsl:with-param name="root" select="$root" />
        <xsl:with-param name="index" select="../@index" />
      </xsl:call-template>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$root/visualkeyboard/keys/key[@unicode=$unicode/@unicode and @shift=$shift/@shift and @vkey=current()/@vkey]">
        <xsl:apply-templates mode="key" select="$root/visualkeyboard/keys/key[@unicode=$unicode/@unicode and @shift=$shift/@shift and @vkey=current()/@vkey]">
          <xsl:with-param name="cap" select="current()/@cap" />
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates mode="blank_key" select="." />
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="position()=last()">
      <xsl:call-template name="specialkeys">
        <xsl:with-param name="root" select="$root" />
        <xsl:with-param name="index" select="5"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="//key" mode="key">
    <xsl:param name="cap" />
    <div class="key plain">
      <xsl:attribute name="id"><xsl:value-of select="@vkey"/></xsl:attribute>
      <div class="keycap"><xsl:value-of select="$cap" /></div>
        <div class="keytext">
          <xsl:choose>
            <xsl:when test="@bitmap">
              <img><xsl:attribute name="src"><xsl:value-of select="$imgpath"/><xsl:value-of select="substring-after(@bitmap,'/')"/></xsl:attribute></img>
            </xsl:when>
            <xsl:otherwise><xsl:value-of select="@text" /></xsl:otherwise>
          </xsl:choose>
        </div>
    </div>
  </xsl:template>

  <xsl:template match="//keys:defkey" mode="blank_key">
    <div class="key plain">
      <xsl:attribute name="id"><xsl:value-of select="@vkey"/></xsl:attribute>
      <div class="keycap"><xsl:value-of select="@cap" /></div>
    </div>
  </xsl:template>
  
</xsl:stylesheet>