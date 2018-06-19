<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_help" select="$locale/Dialog[@Id='Help'][1]" />

	<xsl:template match="/">

		<html>
			<head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />
        <title><xsl:value-of select="$locale/String[@Id='S_HelpTitle']" /></title>
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>config.css</xsl:attribute></link>
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

          #captionBox {
          position: absolute;
          left: 0;
          top: 0;
          width: 100%;
          height: 8px;
          }

          #captionBox div {
          float: left;
          height: 6px;
          }

          #captionBox #c1 {
          width: 60%;
          background: #FC7200;
          }

          #captionBox #c2 {
          width: 22%;
          background: #B92034;
          }

          #captionBox #c3 {
          width: 18%;
          background: #69B7D2;
          }

          #container {
          }

          #header {
          background: white;
          border-bottom: solid 1px #929396;
          top: 8px; left: 0;
          position: absolute;
          width: 100%;
          height: 56px;
          }

          #footer {
          left: 0px;
          position: absolute;
          bottom: 0px;
          height: 40px;
          width: 100%;
          background: #79C3DA;
          }

          #icon {
          vertical-align: middle;
          margin: 10px 15px;
          }

          #exiticon { float: left }

          #title {
          display: inline;
          font-size: 21.3px;
          color: black;
          vertical-align: middle;
          }

          #help {
          font-size: 14.7px;
          padding: 0px;
          position: absolute;
          left: 0; top: 0px;
          width: 100%;
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
          
          #help {
            margin: 60px 0 0 0;
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
				<div id="container">
          <div id="captionBox"><div id="c1"></div><div id="c2"></div><div id="c3"></div></div>
					<div id="header">
						<img id="icon" alt="Help">
							<xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>48.png</xsl:attribute>
						</img>

						<div id="title">
							<xsl:value-of select="$locale/String[@Id='S_HelpTitle']" />
						</div>
					</div>

					<div id="help">
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
                <xsl:call-template name="button">
                  <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
                  <xsl:with-param name="command" select="'keyman:cancel'" />
                </xsl:call-template>
						</div>
					</div>
				</div>							 
			</body>

		</html>
	</xsl:template>
	
</xsl:stylesheet>
