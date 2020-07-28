<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_hint" select="$locale/Dialog[@Id='Usage'][1]" />

	<xsl:template match="/">

		<html>
			<head>
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
				<title>
					<xsl:value-of select="$locale/String[@Id='Usage_Title']" />
				</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />, "Segoe UI"; font-size: 13.3px; }

					body { padding: 6px; margin: 0px; overflow: auto;
					background: #A0C1DC; border: none; }
					html { padding: 0px; margin: 0px; overflow: auto; }

					#container {
					}

					#buttons {
					text-align: center;
					padding: 10px;
					}

					#buttons input {
					x-margin: 4px 4px; font-size: 12px
					}
				</style>
			</head>

			<body>
				<div id="container">
					<p><b><xsl:value-of select="/Keyman/KeyboardName" /></b></p>

					<xsl:choose>
  					<xsl:when test="not(/Keyman/HasWelcome) and not(/Keyman/HasOSK)">
	  				  <p>
                <xsl:copy-of select="($locale/String[@Id='S_Usage_KeyboardNoDocumentation'])[1]" />
					  	</p>
					  </xsl:when>
					  <xsl:otherwise>
					    <p>
                <xsl:copy-of select="($locale/String[@Id='S_Usage_KeyboardDocumentation'])[1]" />
					    </p>
					  </xsl:otherwise>
					</xsl:choose>
					<div id="buttons">

					  <xsl:if test="not(/Keyman/HasWelcome) and not(/Keyman/HasOSK)">
							<input type="button" onclick="javascript:location.href='keyman:help'">
								<xsl:attribute name='Value'>
									<xsl:value-of select="$locale/String[@Id='S_Usage_Help']"/>
								</xsl:attribute>
							</input>
						</xsl:if>

						<xsl:if test="/Keyman/HasWelcome">
							<input type="button" onclick="javascript:location.href='keyman:welcome'">
								<xsl:attribute name='Value'>
									<xsl:value-of select="$locale/String[@Id='S_Usage_Welcome']"/>
								</xsl:attribute>
							</input>
						</xsl:if>

					  <xsl:if test="/Keyman/HasWelcome and /Keyman/HasOSK">
						  &#160; &#160; &#160;
						</xsl:if>

						<xsl:if test="/Keyman/HasOSK">
							<input type="button" onclick="javascript:location.href='keyman:osk'">
								<xsl:attribute name='Value'>
									<xsl:value-of select="$locale/String[@Id='S_Usage_OSK']"/>
								</xsl:attribute>
							</input>
						</xsl:if>
					</div>

				</div>
			</body>
		</html>
	</xsl:template>

</xsl:stylesheet>
