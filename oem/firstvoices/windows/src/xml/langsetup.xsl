<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:variable name="locale_langsetup" select="$locale/Dialog[@Id='LangSetup'][1]" />
	
	<xsl:template match="/">

		<html>
			<head>
				<title>
					<xsl:value-of select="$locale/String[@Id='S_LangSetup_Title']" />
				</title>
				<style type="text/css">
					* { font-family: <xsl:value-of select="($locale/String[@Id='SK_UIFontName'])[1]" />; font-size: 13.3px; }

					body { padding: 0px; margin: 0px; overflow: hidden;
					width: <xsl:value-of select="$locale_langsetup/@Width" />px;
					height: <xsl:value-of select="$locale_langsetup/@Height" />px;
					background: white; border: none; }
					html { padding: 0px; margin: 0px; overflow: hidden; }

					#container, #size {
					width: <xsl:value-of select="$locale_langsetup/@Width" />px;
					height: <xsl:value-of select="$locale_langsetup/@Height" />px;
					}

					#size { position: absolute; }
					
					#header {
					background: #AD4A28;
					top: 0; left: 0; padding: 8px;
					height: 36px; 
					width: 100%;
					white-space: nowrap;
					text-overflow: ellipsis;
					overflow: hidden;
					}

					#footer {
					left: 1px;
					position: absolute;
					bottom: 45px;
					width: 100%;
					}
					
					#footer_buttons {
					left: 1px;
					border-top: solid 1px gray;
					position: absolute;
					bottom: 1px;
					height: 40px;
					width: 100%;
					}

					#icon {
					vertical-align: middle;
					margin: 0 15px;
					}
					
					#tasks {
					
					border-collapse: collapse;
					}
					
					#tasks_container {
						height: 130px;
						overflow-y: auto;
						border: solid 1px gray;
						margin: 12px 8px;
					}
					
					td { vertical-align: middle; margin: 0; padding: 1px 4px }
					td.name { font-weight: bold }
					
					#title {
					display: block;
					font-size: 16px;
					font-weight: bold;
					color: white;
					vertical-align: middle;
					width: <xsl:value-of select="$locale_langsetup/@Width" />px;
					white-space: nowrap;
					text-overflow: ellipsis;
					overflow: hidden;
					}

					#langsetup {
					padding: 20px;
					}

					#checkbox {
					float: left;
					font-size: 12px;
					color: white;
					padding: 12px 10px;
					}

					#buttons {
					float: right;
					padding: 10px;
					}
					
					#blurb, #blurb b {
						font-size: 12px;
					}
					
					#ask {
						font-size: 12px;
						font-weight: bold;
					}
					
					#note {
						font-size: 11px;
						margin: 8px 20px;
					}
					
					#checksettings {
						padding: 10px 0 0 8px;
						float: left;
						}
						
					#buttons input {
						width: 80px; margin: 0 4px;
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
							location.href='keyman:cancel';
						}
					}
          
          function toggleSave(elem)
          {
            var checks = document.getElementById('tasks').getElementsByTagName('input');
            for(var i in checks)
            {
              checks[i].disabled = elem.checked;
              checks[i].checked = !elem.checked;
            }
            
            var checks = document.getElementById('tasks').getElementsByTagName('td');
            for(var i in checks) checks[i].disabled = elem.checked;

            if(elem.checked)
            {
              document.getElementById('button_ok').value = "<xsl:value-of select="$locale/String[@Id='S_Button_OK']" />";
              document.getElementById('button_cancel').disabled = true;
              location.href='keyman:savedisableditems';
            }
            else
            {
              document.getElementById('button_ok').value = "<xsl:value-of select="$locale/String[@Id='S_Button_Apply']" />";
              document.getElementById('button_cancel').disabled = false;
              location.href='keyman:dontsavedisableditems';          
            }
          }
				</script>
			</head>

			<body>	<div id="size"></div>
				<div id="container">
					<div id="header">
						<div id="title">
							<xsl:value-of select="$locale/String[@Id='S_LangSetup_TitleInnerPrefix']"/>&#160;<xsl:value-of select="/Keyman/KeymanSystemConfigItems/scriptnames" />&#160;<xsl:value-of select="$locale/String[@Id='S_LangSetup_TitleInnerSuffix']"/>
						</div>
					</div>
					<div id="langsetup">
						<div id="blurb">
							<xsl:value-of select="$locale/String[@Id='S_LangSetup_BlurbPrefix']"/>
							<b>&#160;<xsl:value-of select="/Keyman/KeymanSystemConfigItems/scriptnames" />&#160;</b>
							<xsl:value-of select="$locale/String[@Id='S_LangSetup_BlurbSuffix']"/>
						</div>

						<div id="tasks_container">
							<table id="tasks">
								<xsl:apply-templates select="//KeymanSystemConfigItem" />
							</table>
						</div>
						
						<div id="ask"><xsl:value-of select="$locale/String[@Id='S_LangSetup_Ask']"/></div>
					</div>

					<div id="footer">
						<div id="note">
							<xsl:value-of select="$locale/String[@Id='S_LangSetup_Note']"/>
						</div>
					</div>
						<div id="footer_buttons">
							<div id="buttons">
								<input type="submit" onclick="javascript:location.href='keyman:ok'" style="width:120px" id="button_ok">
									<xsl:attribute name="value">
										<xsl:value-of select="$locale/String[@Id='S_Button_Apply']"/>
									</xsl:attribute>
								</input>
								<input type="button" onclick="javascript:location.href='keyman:cancel'" style="width:120px" id="button_cancel">
									<xsl:attribute name="value">
										<xsl:value-of select="$locale/String[@Id='S_Button_RemindLater']"/>
									</xsl:attribute>
								</input>
							</div>
							<div id="checksettings">
								<input type="checkbox" onclick="javascript:toggleSave(this);" id="chkSave" />
								<label for="chkSave">
									<xsl:value-of select="$locale/String[@Id='S_LangSetup_SaveDisabledItems']" />
								</label>
							</div>
						</div>
				</div>							 
			</body>

		</html>
	</xsl:template>

	<xsl:template match="//KeymanSystemConfigItem">
		<xsl:if test="not(//disableditem[@id=current()/id])">
			<tr>
				<td>
					<input type="checkbox">
						<xsl:attribute name="onclick">
							javascript:if(this.checked) location.href="keyman:ticktask?id=<xsl:value-of select="index" />"; else location.href="keyman:unticktask?id=<xsl:value-of select="index" />";
						</xsl:attribute>
						<xsl:attribute name="id">
							Task_<xsl:value-of select="index" />
						</xsl:attribute>
						<xsl:if test="not(@DefaultUnchecked)">
							<xsl:attribute name="checked">checked</xsl:attribute>
						</xsl:if>
						<xsl:attribute name="name">
							Task_<xsl:value-of select="index" />
						</xsl:attribute>
					</input>
				</td>
				<td class="name">
					<label>
						<xsl:attribute name="for">
							Task_<xsl:value-of select="index"/>
						</xsl:attribute>
						<xsl:value-of select="$locale/String[@Id=concat('S_LangSetup_',current()/name)]" />
					</label>
				</td>
				<xsl:choose>
					<xsl:when test="detail">
						<td>
						</td>
					</xsl:when>
					<xsl:otherwise>
						<td>
							<a>
								<xsl:attribute name="href">
									keyman:help?name=<xsl:value-of select="name"/>
								</xsl:attribute>
								<xsl:value-of select="$locale/String[@Id='S_LangSetup_MoreInfo']" />
							</a>
						</td>
					</xsl:otherwise>
				</xsl:choose>
			</tr>
			<xsl:if test="detail">
				<tr>
					<td></td>
					<td class="detail">
						<xsl:value-of select="detail" />
					</td>
					<td>
						<a>
							<xsl:attribute name="href">
								keyman:help?name=<xsl:value-of select="name"/>
							</xsl:attribute>
							<xsl:value-of select="$locale/String[@Id='S_LangSetup_MoreInfo']" />
						</a>
					</td>
				</tr>
			</xsl:if>
		</xsl:if>
	</xsl:template>	
</xsl:stylesheet>
