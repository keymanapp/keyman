<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_hint" select="$locale/Dialog[@Id='Hint'][1]" />
	<xsl:variable name="HintTitle">HintTitle_<xsl:value-of select="/Keyman/Hint/@ID" /></xsl:variable>
	<xsl:variable name="Hint">Hint_<xsl:value-of select="/Keyman/Hint/@ID" /></xsl:variable>

	<xsl:template match="/">

		<html>
			<head>
        <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />
				<title><xsl:value-of select="$locale/String[@Id=$HintTitle]" /></title>
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>config.css</xsl:attribute></link>
        <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="/Keyman/templatepath"/>hint.css</xsl:attribute></link>
				<style type="text/css">
					* { 
            font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />;
          }
					#container {
  					width: <xsl:value-of select="$locale_hint/@Width" />px;
	  				height: <xsl:value-of select="$locale_hint/@Height" />px;
          }

        </style>
				<script type="text/javascript">
					document.onkeydown = function()
					{
						if(event.keyCode == 13 <![CDATA[&&]]> (!event.srcElement.type || event.srcElement.type != 'button'))
						{
							event.cancelBubble = true; event.returnValue = false;
							location.href='keyman:ok';
						}
						else if(event.keyCode == 27 )
						{
							event.cancelBubble = true; event.returnValue = false;
							<xsl:choose>
								<xsl:when test="/Keyman/Buttons/Button[@ID='Cancel']">location.href='keyman:cancel';</xsl:when>
								<xsl:otherwise>location.href='keyman:ok';</xsl:otherwise>
							</xsl:choose>
						}
					}
				</script>
			</head>

			<body>
				<div id="container">
          <div id="captionBox"><div id="c1"></div><div id="c2"></div><div id="c3"></div></div>

					<div id="header">
						<img id="icon" src="hints.png" alt="Hint">
							<xsl:attribute name="src">
								<xsl:value-of select="/Keyman/templatepath"/>48.png
							</xsl:attribute>
						</img>

						<div id="title">
							<xsl:value-of select="$locale/String[@Id=$HintTitle]" />
						</div>
					</div>

					<div id="hint">
						<xsl:apply-templates select="/Keyman/Hint" />
					</div>
					
					<div id="footer">
						<div id="checkbox">
							<input type="checkbox" onclick='javascript:if(this.checked) location.href="keyman:dontshowhint"; else location.href="keyman:showhint";' 
										 style="vertical-align: middle;" id="chkWelcome" />&#160;<label for="chkWelcome"><xsl:value-of select="$locale/String[@Id='S_HintDialog_DontShowHintAgain']"/></label>
						</div>

						<div id="buttons">
							<xsl:if test="/Keyman/Buttons/Button[@ID='OK']">
                <xsl:call-template name="button">
                  <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_OK']"/></xsl:with-param>
                  <xsl:with-param name="default" select="1" />
                  <xsl:with-param name="command" select="'keyman:ok'" />
                </xsl:call-template>
							</xsl:if>
							<xsl:if test="/Keyman/Buttons/Button[@ID='Cancel']">
                <xsl:call-template name="button">
                  <xsl:with-param name="caption"><xsl:value-of select="$locale/String[@Id='S_Button_Cancel']"/></xsl:with-param>
                  <xsl:with-param name="command" select="'keyman:cancel'" />
                </xsl:call-template>
							</xsl:if>
						</div>
					</div>
				</div>							 
			</body>

		</html>
	</xsl:template>

	<xsl:template match="Hint">
		<xsl:value-of select="$locale/String[@Id=$Hint]" />
	</xsl:template>

<!--
  Example of how to do a custom hint
	
	<xsl:template match="Hint[@ID='KH_EXITPRODUCT']">
		<img id="exiticon" alt="icon">
			<xsl:attribute name="src">
				<xsl:value-of select="/Keyman/templatepath"/>keyman_48x48.png
			</xsl:attribute>
		</img>
		<xsl:value-of select="$locale/String[@Id=$Hint]" />
	</xsl:template>
-->
	
</xsl:stylesheet>
