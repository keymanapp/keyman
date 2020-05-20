<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_hint" select="$locale/Dialog[@Id='Usage'][1]" />

	<xsl:template match="/">

		<html>
			<head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <script src="/app/sentry.bundle.min.js"></script>
        <script src="/app/sentry.init.js"></script>
				<title>
					<xsl:value-of select="$locale/String[@Id='SK_Usage_Title']" />
				</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />, "Segoe UI"; font-size: 13.3px; }

					body { padding: 6px; margin: 0px;
					background: #A0C1DC; border: none; }
					html { padding: 0px; margin: 0px;  }

					#toolbar {
						border-left: solid 1px black;
						border-right: solid 1px black;
						border-bottom: solid 1px black;
						position: absolute;
						left: <xsl:value-of select="/Keyman/Toolbar/@Left" />px;
						width: <xsl:value-of select="/Keyman/Toolbar/@Width" />px;
						top: 0px;
						height: 12px;
						}

					#buttons {
					text-align: center;
					padding: 10px;
					}

					#buttons input {
					x-margin: 4px 4px; font-size: 12px
					}

					#outer { position: relative; }
					#middle { position: absolute; top: 50%; }
					#inner { position: relative; top: -30%; width: 100%; font-size: 13.3px; text-align: center; }

					#nokeyboards { width: 84%; padding: 8px 3%; margin-left: 5%; background: #F5F6BE; border: solid 1px gray; text-align: left }
					#nokeyboards img { float: left; padding: 0 16px 0 0 }

				</style>
			</head>

			<body>
				<div id="outer">
					<div id="middle">
						<div id="inner">
							<xsl:choose>
								<xsl:when test="/Keyman/Keyboards/Keyboard">
									<p>
                    <xsl:copy-of select="($locale/String[@Id='S_Usage_KeymanOff'])[1]" />
                  </p>
									<div id="buttons">
										<input type="button" onclick="javascript:location.href='keyman:help'">
											<xsl:attribute name='Value'>
												<xsl:value-of select="$locale/String[@Id='S_Usage_Help']"/>
											</xsl:attribute>
										</input>
									</div>
								</xsl:when>
								<xsl:otherwise>
									<div id="nokeyboards">
										<img src="/app/info.png" />
                    <xsl:copy-of select="($locale/String[@Id='S_Usage_NoKeyboardsInstalled'])[1]" />
									</div>
									<p></p>
									<div id="buttons">
										<input type="button" onclick="javascript:location.href='keyman:config'">
											<xsl:attribute name='Value'>
												<xsl:value-of select="$locale/String[@Id='S_Usage_Config']"/>
											</xsl:attribute>
										</input>
										<input type="button" onclick="javascript:location.href='keyman:help'">
											<xsl:attribute name='Value'>
												<xsl:value-of select="$locale/String[@Id='S_Usage_Help']"/>
											</xsl:attribute>
										</input>
									</div>
								</xsl:otherwise>
							</xsl:choose>
						</div>
					</div>
				</div>
			</body>
		</html>
	</xsl:template>

</xsl:stylesheet>
