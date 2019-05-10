<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:include href="elements.xsl" />

  <xsl:variable name="locale_installkeyboard" select="$locale/Dialog[@Id='InstallKeyboard'][1]" />
  
  <xsl:template match="/">
    <html>
      <head>
        <title><xsl:value-of select="$locale/String[@Id='S_InstallKeyboard_Title']"/></title>
        <style type="text/css">
          * { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; }
        
          html, body { overflow: hidden }
          div { 
            position: absolute;
          }
          
          #background { z-index: 1; left: 0; top: 0; width: <xsl:value-of select="$locale_installkeyboard/@Width" />px; height: <xsl:value-of select="$locale_installkeyboard/@Height" />px }
          #foreground { z-index: 2; left: 0; top: 0; width: <xsl:value-of select="$locale_installkeyboard/@Width" />px; height: <xsl:value-of select="$locale_installkeyboard/@Height" />px }

          #frame {
            top: 0; left: 0; 
            width: <xsl:value-of select="$locale_installkeyboard/@Width - 20" />px; height: 244px; 
            padding: 10px; 
          }
          
          #image {
            top: 0; 
            left: 0; 
            height: 250px; 
            width: 140px; 
            padding: 5px
          }
          
          .content {
            left: 150px; top: 0px; 
            width: 438px; 
            padding: 4px;
            background: white;
            border-left: 2px solid #888888;
            border-top: 2px solid #888888;
            border-right: 2px solid #888888;
            overflow-y: auto;
          <xsl:choose>
            <xsl:when test="/Keyman/KeymanPackageFile/readme">
              height: 226px; visibility: hidden;
          }
          .content_selected {
            left: 150px; top: 0px; 
            width: 438px; 
            padding: 4px;
            background: white;
            border-left: 2px solid #888888;
            border-top: 2px solid #888888;
            border-right: 2px solid #888888;
            overflow-y: auto;
            height: 226px; 
            visibility: visible
          }
          .content_selected#readme {
            padding: 0px; width: 446px; height: 234px; overflow: hidden;
          }
          #tabs {
            cursor: default;
            left: 150px; top: 236px;
            width: 448px; background: none;
            border-top: 2px solid #888888;
            padding-left: 2px;
          }
          #tabs div {
            float: left;
            position: relative;
            font-weight: bold;
            font-size: 13.3px;
            padding: 2px;
            width: 76px; background: none; text-align: center;
            top: -2px; margin-left: -2px;
            border-left: solid 2px #888888;
            border-right: solid 2px #888888;
            border-bottom: solid 2px #888888;
            }
          #tabs div.selected {
            top: -2px;
            background: white;
            border-left: solid 2px #888888;
            border-right: solid 2px #888888;
            border-bottom: solid 2px #888888;
            }
            </xsl:when>
            <xsl:otherwise>
              height: 244px;
              border-bottom: solid 2px #888888;
          }
            </xsl:otherwise>
          </xsl:choose>

          #details {
          }
          
          #readme {
          }
          
          #footer {
            top: 260px; left: 0;
            width: <xsl:value-of select="$locale_installkeyboard/@Width - 16" />px; height: 26px;
            vertical-align: middle;
            padding: 8px;
            text-align: right;
          }
          
          .keyboardname {
            font-weight: bold;
            font-size: 14.7px;
            padding: 4px;
            background: #ceedfb;
          }
          .detailheader {
            display: inline;
            white-space: nowrap;
            font-weight: bold;
            font-size: 13.3px;
            padding-right: 10px;
            margin: 1px;
            vertical-align: top;
          }
          .otherdetails {
            font-size: 13.3px;
            display: inline;
            vertical-align: top;
          }
        </style>
        <script type="text/javascript">
          function tabClick(tab)
          {
            var elemDetails = document.getElementById('details');
            var elemReadme = document.getElementById('readme');
            var tabDetails = document.getElementById('tabDetails');
            var tabReadme = document.getElementById('tabReadme');
            if(tab == tabDetails)
            {
              tabDetails.className = "selected";
              tabReadme.className = "";
              elemDetails.className = "content_selected";
              elemReadme.className = "content";
            }
            else
            {
              tabDetails.className = "";
              tabReadme.className = "selected";
              elemDetails.className = "content";
              elemReadme.className = "content_selected";
            }
          }
	
					function genkeydown(event)
					{
						if(!event) return true;
						if(event.keyCode == 13 <![CDATA[&&]]> event.srcElement.tagName != 'A' <![CDATA[&&]]> (!event.srcElement.type || event.srcElement.type != 'button')) location.href='keyman:keyboard_install';
						else if(event.keyCode == 27) location.href='keyman:keyboard_cancel';
						else return true;
						event.cancelBubble = true; event.returnValue = false;
						return false;
					}
					
					function framekeydown()
					{
						genkeydown(document.getElementById('frameReadme').contentWindow.event);
					}
					
					document.onkeydown = function() { return genkeydown(event); }
					
					function setupframehotkeys()
					{
            try
            {
  						var e = document.getElementById('frameReadme').contentWindow.document;
	  					e.onkeydown = framekeydown;
            } catch(ex) { /* I1672 - ignore access denied when loading other links */ }
					}
					
        </script>

      </head>
      <body>
        <xsl:if test="/Keyman/KeymanPackageFile/readme">
          <!-- fire startup event to select the details tab - only if we have a readme file -->
          <xsl:attribute name="onload">javascript:tabClick(document.getElementById('tabDetails'))</xsl:attribute>
        </xsl:if>
        <div id="background"><img style='width:100%; height: 100%'><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>gradient.gif</xsl:attribute></img></div>
        <div id="foreground">
          <div id="frame">
            <div id="image">
              <img style="height: 250px; width: 140px;">
                <xsl:choose>
                  <xsl:when test="/Keyman/KeymanKeyboardFile">
                    <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>defaultkeyboard.gif</xsl:attribute>
                  </xsl:when>
                  <xsl:when test="/Keyman/KeymanPackageFile/graphic">
                    <xsl:attribute name="src"><xsl:value-of select="/Keyman/imagepath"/><xsl:value-of select="/Keyman/KeymanPackageFile/graphic" /></xsl:attribute>
                    <xsl:attribute name="style">height: 250px; width: 140px; border: solid 1px black</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>defaultpackage.gif</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
              </img>
            </div>
            <xsl:if test="/Keyman/KeymanPackageFile/readme">
              <!-- add tabs to the dialog for details and readme -->
              <div id="tabs">
                <div id="tabDetails" onclick="javascript:tabClick(this)"><xsl:value-of select="$locale/String[@Id='S_InstallKeyboard_Tab_Details']"/></div>
                <div id="tabReadme" onclick="javascript:tabClick(this)"><xsl:value-of select="$locale/String[@Id='S_InstallKeyboard_Tab_Readme']"/></div>
              </div>
              <div class="content" id="readme">
                <iframe style="visibility: visible; width:446px; height:100%; padding: 0px; margins: 0px" scrolling="auto" frameborder="no" id="frameReadme" onload="setupframehotkeys()">
                  <xsl:attribute name="src"><xsl:value-of select="/Keyman/KeymanPackageFile/readme" /></xsl:attribute>
                </iframe>
              </div>
            </xsl:if>              
            <div class="content" id="details">
              <div style="display:block;position:static;margin:0;padding:0;background:blue;overflow:hidden;">
              <table style="width: 100%;background:white;">
                <tr>
                  <td colspan="2" class="keyboardname">
                    <xsl:choose>
                      <xsl:when test="/Keyman/KeymanKeyboardFile/bitmap">
                        <img style="margin-right: 6px; float: left; height: 16px; width: 16px;">
                          <xsl:attribute name="src"><xsl:value-of select="/Keyman/KeymanKeyboardFile/bitmap"/></xsl:attribute>
                        </img>
                      </xsl:when>
                      <xsl:otherwise>
                        <img style="margin-right: 6px; float: left; height: 16px; width: 16px;">
                          <xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>no_icon.gif</xsl:attribute>
                        </img>
                      </xsl:otherwise>
                    </xsl:choose>
                    <xsl:value-of select="/Keyman/KeymanKeyboardFile/keyboardname" />
                    <xsl:value-of select="/Keyman/KeymanPackageFile/description" />
                  </td>
                </tr>

                <xsl:apply-templates select="/Keyman/KeymanKeyboardFile" />
                <xsl:apply-templates select="/Keyman/KeymanPackageFile" />
              </table>
              </div>
            </div>
          </div>
          <div id="footer">
						<xsl:if test="/Keyman/allowelevate">
							<xsl:call-template name="button">
								<xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_InstallKeyboard_Button_InstallAllUsers']"/></xsl:with-param>
								<xsl:with-param name="command">keyman:keyboard_installallusers</xsl:with-param>
								<xsl:with-param name="width">120px</xsl:with-param>
                <xsl:with-param name="shield">1</xsl:with-param>
							</xsl:call-template>
						</xsl:if>
            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_InstallKeyboard_Button_Install']"/></xsl:with-param>
							<xsl:with-param name="default">1</xsl:with-param>
              <xsl:with-param name="command">keyman:keyboard_install</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="button">
              <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
              <xsl:with-param name="command">keyman:keyboard_cancel</xsl:with-param>
              <xsl:with-param name="width">70px</xsl:with-param>
            </xsl:call-template>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="/Keyman/KeymanPackageFile/KeymanKeyboardsPackageFile/KeymanKeyboardFile/keyboardname">
    <xsl:value-of select="."/><br />
  </xsl:template>

  <xsl:template match="/Keyman/KeymanPackageFile/Fonts/Font/name">
    <xsl:value-of select="."/><br />
  </xsl:template>
  
  <xsl:template match="/Keyman/KeymanKeyboardFile">
    <tr>
      <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Filename']"/></td>
      <td class="otherdetails"><xsl:value-of select="filename" /></td>
    </tr>

    <tr>
      <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Encodings']"/></td>
      <td class="otherdetails">
        <xsl:for-each select="encodings/encoding">
          <xsl:value-of select="."/>
          <xsl:if test="not(last())">,</xsl:if>
        </xsl:for-each>
      </td>
    </tr>

    <xsl:if test="copyright">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Copyright']"/></td>
        <td class="otherdetails"><xsl:value-of select="copyright" /></td>
      </tr>
    </xsl:if>
    
    <xsl:if test="message">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Message']"/></td>
        <td class="otherdetails"><xsl:value-of select="message" /></td>
      </tr>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="/Keyman/KeymanPackageFile">
    <tr>
      <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Keyboards']"/></td>
      <td class="otherdetails"><xsl:apply-templates select="KeymanKeyboardsPackageFile/KeymanKeyboardFile/keyboardname" /></td>
    </tr>
    
    <xsl:if test="Fonts/Font">
      <tr>
        <td class="detailheader"><xsl:value-of select="$locale/String[@Id='S_Caption_Fonts']"/></td>
        <td class="otherdetails"><xsl:apply-templates select="Fonts/Font/name" /></td>
      </tr>
    </xsl:if>
    
    <xsl:apply-templates select="detail[@name='version']"><xsl:with-param name="header"><xsl:value-of select="$locale/String[@Id='S_Caption_Version']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='author']"><xsl:with-param name="header"><xsl:value-of select="$locale/String[@Id='S_Caption_Author']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='website']"><xsl:with-param name="header"><xsl:value-of select="$locale/String[@Id='S_Caption_Website']"/></xsl:with-param></xsl:apply-templates>
    <xsl:apply-templates select="detail[@name='copyright']"><xsl:with-param name="header"><xsl:value-of select="$locale/String[@Id='S_Caption_Copyright']"/></xsl:with-param></xsl:apply-templates>
    
  </xsl:template>
  
  <xsl:template match="detail">
    <xsl:param name="header" />
    <tr>
      <td class="detailheader"><xsl:value-of select="$header" /></td>
      <td class="otherdetails">
        <xsl:choose>
          <xsl:when test="@url">
            <a>
              <xsl:attribute name="href">keyman:link?url=<xsl:value-of select="@url"/></xsl:attribute>
              <xsl:attribute name="title"><xsl:value-of select="@url"/></xsl:attribute>
              <xsl:value-of select="." />
            </a>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="." />
          </xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>    
  </xsl:template>

</xsl:stylesheet>