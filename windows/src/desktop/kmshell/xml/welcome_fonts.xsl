<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:template match="/">

		<html>
			<head>
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
				<title>fonts</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/string[@name='SK_UIFontName'])[1]" />, "Segoe UI"; font-size: 13.3px; }

					body { padding: 24px 16px; margin: 0px; overflow: auto; height: 100%; background: #A0C1DC; border: none; }
					html { padding: 0px; margin: 0px; overflow: auto; }
					table { margin: 12px 0 }
					td.font { font-size: 13.3px; font-weight: bold }
					td.coverage { font-size: 13.3px }

				</style>

			</head>

			<body>
				<xsl:choose>
          <xsl:when test="/Keyman/not_keyman">
            <div><xsl:value-of select="$locale/string[@name='S_FontHelper_WindowsKeyboard']"/></div>
          </xsl:when>
				  <xsl:when test="not(/Keyman/Fonts)">
				    <div><xsl:value-of select="$locale/string[@name='S_FontHelper_NonUnicodeKeyboard']"/></div>
				  </xsl:when>
					<xsl:when test="count(/Keyman/Fonts/Font[@Coverage &gt; 90]) > 0">
						<div><xsl:value-of select="$locale/string[@name='S_FontHelper_MatchedFonts']"/></div>
						<div>
							<table>
								<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &gt; 90]" />
							</table>
						</div>
						<xsl:if test="count(/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt; 50]) &gt; 0">
							<div><xsl:value-of select="$locale/string[@name='S_FontHelper_PossibleFonts']"/></div>
							<div>
								<table>
									<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]" />
								</table>
							</div>
						</xsl:if>
					</xsl:when>
					<xsl:when test="count(/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]) > 0">
						<div><xsl:value-of select="$locale/string[@name='S_FontHelper_PossibleFontsOnly']"/></div>
						<div>
							<table>
								<xsl:apply-templates select="/Keyman/Fonts/Font[@Coverage &lt;= 90 and @Coverage &gt;= 50]" />
							</table>
						</div>
					</xsl:when>
					<xsl:otherwise>
						<div><xsl:copy-of select="$locale/string[@name='S_FontHelper_NoFonts'][1]"/></div>
					</xsl:otherwise>
				</xsl:choose>

				<div style="margin-top: 24px">
          <xsl:copy-of select="$locale/string[@name='S_FontHelper_Hint1']"/>
          <xsl:text xml:space="preserve"> </xsl:text>
          <img style="vertical-align: middle; margin: 0 2px 0 0" src="/app/fonthinticon.gif" />
          <xsl:text xml:space="preserve"> </xsl:text>
          <xsl:copy-of select="$locale/string[@name='S_FontHelper_Hint2']"/>
        </div>
			</body>

		</html>
	</xsl:template>

	<xsl:template match="Font">
		<tr>
			<td>
				<a>
					<xsl:attribute name="href">keyman:selectfont?font=<xsl:value-of select="@Name"/></xsl:attribute>
					<xsl:value-of select="@Name" />
				</a>
			</td>
			<td>
				<xsl:value-of select="@Coverage" />%
			</td>
		</tr>
	</xsl:template>
</xsl:stylesheet>