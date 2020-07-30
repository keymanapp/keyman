<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd" method="html" encoding="utf-8" standalone="yes" />

	<xsl:include href="elements.xsl"/>

	<xsl:template match="/">

		<html>
			<head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />

				<title>fonts</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />; font-size: 12px; }

					body { padding: 0px 0px; margin: 0px; width: 100%; height: 100%; background: #A0C1DC; border: none;}
					html { padding: 0px; margin: 0px; overflow: auto; }
					table { margin: 12px 0; border-collapse: collapse; width: <xsl:value-of select="count(/Keyman/Chars/Ch) * 15 + 250" />px; }
					table tr td.font { text-align: left; vertical-align: middle; font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />; white-space: nowrap; font-size: 11px; font-weight: bold; margin: 0px; padding: 0px; border-left: none; border-right: none; }
					table tr td.coverage { text-align: left; vertical-align: middle; font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />; white-space: nowrap; font-size: 11px; margin: 0px; padding-left: 6px; border-left: none; }
          
          td.char { text-align: center; vertical-align: middle; border: solid 1px #80A1BC; font-size: 13.3px; background: white; }
          td.excluded { background: #FFcfc0; }
          td:hover { background: #80A1BC; color: white; }
					
					#outer { position: absolute; height: 100%; left:0; top: 0; width: 100% }
					#middle { position: absolute; top: 50%; width: 100% }
					#inner { position: relative; top: -50%; }
					
					#inner-nokeyboards { position: relative; top: -50%; margin: -24px 5% 0 5%; min-height: 32px; width: 84%; padding: 8px 3%; background: #F5F6BE; border: solid 1px gray; text-align: left }
					#inner-nokeyboards img { float: left; padding: 0 16px 0 0 }
          
          col.index { width: auto !important; }
          col { width: 14px; color: #202428;  }
					
					#search { text-align: center; width: 100%; }
					#no { text-align: center; width: 100% }
					#fonts { padding: 12px }
          
          <xsl:for-each select="/Keyman/Fonts/Font">
            .font_<xsl:value-of select="@Index"/> td { font-family: "<xsl:value-of select="@Name"/>"; }
          </xsl:for-each>

				</style>

        <script type="text/javascript">
          document.onclick = function(event)
          {
            if(!event) event = window.event;
            var elem = event.srcElement ? event.srcElement : event.target;
            if(elem.className.indexOf('char') &lt; 0) return false;
            if(elem.title == '') return false;
            location.href = 'keyman:insertchars?chars=x'+elem.title.substring(2);
            return true;
          }
        </script>
 
			</head>

			<body>
				<xsl:choose>
					<xsl:when test="/Keyman/Searching">
						<div id="outer">
							<div id="middle">
								<div id="inner">
									<div id="search">
										<img alt="Searching...">
											<xsl:attribute name="src">
												<xsl:value-of select="/Keyman/templatepath"/>search.gif
											</xsl:attribute>
										</img>
                    <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_PleaseWait1']"/>
                    <xsl:text xml:space="preserve"> </xsl:text>
										<b><xsl:value-of select="/Keyman/Keyboard/@Name" /></b>
                    <xsl:text xml:space="preserve"> </xsl:text>
                    <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_PleaseWait2']"/>
									</div>
 								</div>
							</div>
						</div>
					</xsl:when>
					<xsl:when test="/Keyman/Keyboard">
							<xsl:choose>
							  <xsl:when test="not(/Keyman/Fonts)">
							    <div id="fonts">
                  <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_NonUnicode1']"/>
                  <xsl:text xml:space="preserve"> </xsl:text>
							    <b><xsl:value-of select="/Keyman/Keyboard/@Name" /></b> 
                  <xsl:text xml:space="preserve"> </xsl:text>
                  <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_NonUnicode2']"/>
							    </div>
							  </xsl:when>
								<xsl:when test="count(/Keyman/Fonts/Font[@Coverage &gt; 90]) > 0">
									<div id="fonts">
										<div>
                      <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_MatchedFonts1']"/>
                      <xsl:text xml:space="preserve"> </xsl:text>
											<b><xsl:value-of select="/Keyman/Keyboard/@Name" /></b>
                      <xsl:text xml:space="preserve"> </xsl:text>
                      <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_MatchedFonts2']"/>
										</div>
										<div>
											<table id="tableHighCover">
                        <colgroup><col class="index" /><col class="index" /><xsl:for-each select="/Keyman/Chars/Ch"><col /></xsl:for-each></colgroup>
												<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &gt; 90]" />
											</table>
										</div>
										<xsl:if test="count(/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]) > 0">
											<div>
                        <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_PossibleFonts']"/>
											</div>
											<div>
												<table id="tableLowCover">
                          <colgroup><col class="index" /><col class="index" /><xsl:for-each select="/Keyman/Chars/Ch"><col /></xsl:for-each></colgroup>
													<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]" />
												</table>
											</div>
										</xsl:if>
									</div>
								</xsl:when>
								<xsl:when test="count(/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]) > 0">
									<div id="fonts">
										<div>
                      <!--<input type="button" onclick="keyman:openinbrowser" value="View this list in your browser" />-->
                      <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_PossibleFontsOnly1']"/>
											<xsl:text xml:space="preserve"> </xsl:text>
                      <b><xsl:value-of select="/Keyman/Keyboard/@Name" /></b>
                      <xsl:text xml:space="preserve"> </xsl:text>
                      <xsl:value-of select="$locale/string[@name='S_OSK_FontHelper_PossibleFontsOnly2']"/>
										</div>
										<div>
											<table id="tableLowCover">
                        <colgroup><col class="index" /><col class="index" /><xsl:for-each select="/Keyman/Chars/Ch"><col /></xsl:for-each></colgroup>
												<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]" />
											</table>
										</div>
									</div>
								</xsl:when>
								<xsl:otherwise>
									<div id="outer">
										<div id="middle">
											<div id="inner">
												<div id="no">
                          <xsl:copy-of select="$locale/string[@name='S_OSK_FontHelper_NoFonts1a'][1]" />
                          <xsl:text xml:space="preserve"> </xsl:text>
													<b><xsl:value-of select="/Keyman/Keyboard/@Name" /></b>
                          <xsl:copy-of select="$locale/string[@name='S_OSK_FontHelper_NoFonts1b'][1]" />
												</div>
											</div>
										</div>
									</div>
								</xsl:otherwise>
							</xsl:choose>
					</xsl:when>
					<xsl:when test="/Keyman/NoKeyboards">
						<div id="outer">
							<div id="middle" style="width: 100%">
								<div id="inner-nokeyboards">
									<img>
										<xsl:attribute name="src">
											<xsl:value-of select="/Keyman/templatepath"/>info.png
										</xsl:attribute>
									</img>
                  <xsl:copy-of select="$locale/string[@name='S_OSK_FontHelper_NoKeyboards'][1]" />
									
								</div>
							</div>
						</div>
					</xsl:when>
					<xsl:otherwise>
						<div id="outer">
							<div id="middle">
								<div id="inner">
									<div id="no"><xsl:copy-of select="$locale/string[@name='S_OSK_FontHelper_ChooseKeyboard'][1]" /></div>
								</div>
							</div>
						</div>
					</xsl:otherwise>
				</xsl:choose>

			</body>
			
		</html>
	</xsl:template>

	<xsl:template match="Font">
		<tr>
      <xsl:attribute name="class">font_<xsl:value-of select="@Index"/></xsl:attribute>
			<td class="font">
				<xsl:value-of select="@Name" />
			</td>
			<td class="coverage">
				<xsl:value-of select="@Coverage" />%
			</td>

      <xsl:variable name="ExcludedChars" select="ExcludedChars" />
      <xsl:for-each select="/Keyman/Chars/Ch">
        <td>
          <!--<xsl:attribute name="onclick">javascript:location.href='keyman:insertchars?chars=x<xsl:value-of select="normalize-space(@CharCode)"/>';</xsl:attribute>-->
          <xsl:attribute name="class">char <xsl:if test="$ExcludedChars/Ch[@CharCode = current()/@CharCode]">excluded</xsl:if></xsl:attribute>
          <xsl:attribute name="title">U+<xsl:value-of select="normalize-space(@CharCode)"/></xsl:attribute>
          <xsl:value-of select="normalize-space(.)"/>
        </td>
      </xsl:for-each>
		</tr>
	</xsl:template>
</xsl:stylesheet>
