<?xml version="1.0" encoding="utf-8" ?>
  
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="elements.xsl"/>
  
  <xsl:template match="/">

		<html>
			<head>
				<META http-equiv="Page-Enter" CONTENT="progid:DXImageTransform.Microsoft.Pixelate(Duration=1)" />

				<title>hintbar</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; font-size: 16px; }

					body { padding: 8px 16px; margin: 0px; overflow: hidden; background: #A0C1DC; border: none; }
					html { padding: 0px; margin: 0px; overflow: hidden; }

				</style>
			</head>

			<body>
			  <table cellspacing="0" cellpadding="0"><tr><td>
        <img style="vertical-align: middle; margin: 0 8px 0 0" onclick="javascript:hinticon()">
					<xsl:attribute name="src">
						<xsl:value-of select="/Keyman/templatepath"/>welcome_info.png
					</xsl:attribute>
				</img>
				</td><td>
        <xsl:copy-of select="$locale/String[@Id='S_Hintbar_NoKeyboards']" />
				</td></tr></table>
			</body>
		</html>
      </xsl:template>
</xsl:stylesheet>
