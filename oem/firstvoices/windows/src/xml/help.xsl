<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_help" select="$locale/Dialog[@Id='Help'][1]" />

	<xsl:template match="/">

		<html>
			<head>
				<title>
					<xsl:value-of select="$locale/String[@Id='S_HelpTitle']" />
				</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; font-size: 13.3px; }

					body { padding: 0px; margin: 0px; overflow: hidden;
					width: <xsl:value-of select="$locale_help/@Width" />px;
					height: <xsl:value-of select="$locale_help/@Height" />px;
					background: white; border: none; }
					html { padding: 0px; margin: 0px; overflow: hidden; }

					#container {
					width: 100%;
					height: 100%;
					}
					
					#size { position: absolute; left: 0; top: 0;
						width: <xsl:value-of select="$locale_help/@Width" />px;
						height: <xsl:value-of select="$locale_help/@Height" />px;
					}
					#container {
										border: 1px solid grey;
                    top: 0;
                    position: absolute;
					}

					#header {
					background: #AD4A28;
					top: 0; left: 0;
					height: 70px;
					}

					#footer {
					left: 1px;
					position: absolute;
					bottom: 1px;
					height: 40px;
					width: 100%;
					background: #AD4A28;
					}

					#icon {
					vertical-align: middle;
					margin: 0 15px;
					}
					
					#exiticon { float: left }

					#title {
					display: inline;
					font-size: 21.3px;
					color: white;
					vertical-align: middle;
					}

					#help {
					font-size: 14.7px;
					padding: 0px;
					text-align: center;
					border: none;
					}

					#checkbox {
					float: left;
					font-size: 13.3px;
					color: white;
					padding: 12px 10px;
					}

					#buttons {
					float: right;
					padding: 10px;
					}
					
					#help div {
						margin: 24px;
						}
						
					#help a { font-size: 18.7px }
					
					#buttons input {
						width: 80px; margin: 0 4px;
						}
				</style>
				<script type="text/javascript">
					document.onkeydown = function()
					{
						if(event.keyCode == 27)
						{
							event.cancelBubble = true; event.returnValue = false;
							location.href='keyman:cancel';
						}
					}
				</script>
			</head>

			<body>
				<div id="size"></div>
				<div id="container" cellpadding="0">
					<div id="header">
						<img id="icon" alt="Help">
							<xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>help.gif</xsl:attribute>
						</img>

						<div id="title">
							<xsl:value-of select="$locale/String[@Id='S_HelpTitle']" />
						</div>
					</div>

					<div id="help">
						<div>
							<a href="javascript:location.href='keyman:opentutorial'">
								<xsl:value-of select="$locale/String[@Id='S_Help_Tutorial']" />
							</a>
						</div>
						<xsl:if test="/Keyman/Keyboard">
							<div>
								<a href="javascript:location.href='keyman:openkeyboardhelp'">
									<xsl:value-of select="$locale/String[@Id='S_Help_Keyboard_Prefix']" />
									<xsl:value-of select="/Keyman/Keyboard/@Name"/>
									<xsl:value-of select="$locale/String[@Id='S_Help_Keyboard_Suffix']" />
								</a>
							</div>
						</xsl:if>
						<div>
							<a href="javascript:location.href='keyman:openproducthelp'">
								<xsl:value-of select="$locale/String[@Id='S_Help_Product']" />
							</a>
						</div>
					</div>
					
					<div id="footer">
						<div id="buttons">
							<input type="button"  onclick="javascript:location.href='keyman:cancel'">
								<xsl:attribute name="value">
									<xsl:value-of select="$locale/String[@Id='S_Button_Cancel']" />
								</xsl:attribute>
							</input>
						</div>
					</div>
				</div>							 
			</body>

		</html>
	</xsl:template>
	
</xsl:stylesheet>
