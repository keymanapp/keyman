<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:msxsl="urn:schemas-microsoft-com:xslt" exclude-result-prefixes="msxsl"
>
    <xsl:output method="html" indent="yes" encoding="utf-8" />

    <xsl:template match="/TouchLayoutBuilder">
        <xsl:text disable-output-escaping='yes'>&lt;!DOCTYPE html></xsl:text>
        <html>
            <head>
                <meta charset="UTF-8" />
                <script src="/app/lib/sentry/bundle.min.js"><xsl:text> </xsl:text></script>
                <script src="/app/lib/sentry/init.js"><xsl:text> </xsl:text></script>
                <title>On Screen Keyboard Builder</title>
                <link rel="stylesheet" type='text/css'><xsl:attribute name="href"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-ui.css</xsl:attribute></link>
                <link rel='stylesheet' type='text/css'><xsl:attribute name="href"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder.css</xsl:attribute></link>
                <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-1.10.2.js</xsl:attribute></script>
                <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-ui.js</xsl:attribute></script>
                <script>var KVKL = <xsl:value-of select='/TouchLayoutBuilder/LayoutJS' />;</script>
                <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder.js</xsl:attribute></script>
            </head>
            <body>
              <div id="wedgeAddRowAbove" class="kcontrol wedge-horz"><span>+</span></div>
              <div id="wedgeAddRowBelow" class="kcontrol wedge-horz"><span>+</span></div>
              <div id="wedgeAddKeyLeft" class="kcontrol wedge-vert"><span>+</span></div>
              <div id="wedgeAddKeyRight" class="kcontrol wedge-vert"><span>+</span></div>
              <div id="btnDelKey">x</div>

              <div id='toolbar'>
                    <div id='controlToolbar'>
                        <button id='btnTemplate'>Template...</button>
                        <button id='btnImport'>Import from On Screen</button>
                    </div>
                    <div id='layoutToolbar'>
                        <div>
                          Platform: <select id='selPlatform'></select>
                          <button id='btnAddPlatform'>Add</button>
                          <button id='btnDelPlatform'>Del</button>
                          <input type='checkbox' id='chkDisplayUnderlying'/> <label for='chkDisplayUnderlying'>Display underlying</label>
                        </div>
                        <div>
                          Layer: <select id='selLayer'></select>
                          <button id='btnAddLayer'>Add</button>
                          <button id='btnDelLayer'>Del</button>
                          <button id='btnEditLayer'>Edit</button>
                        </div>
                        <div>
                          Presentation: <select id='selPlatformPresentation'></select>
                          <input type='checkbox' id='chkShowAllModifierOptions'/> <label for='chkShowAllModifierOptions'>Show all modifier options</label>
                        </div>
                    </div>
                    <br class='clear' />
                </div>
                <div id='kbd'></div>
              <div id='keyToolbar'>
                <label for='selKeyType'>Key Type:</label> <select id='selKeyType'>
                  <option value='0'>Default</option>
                  <option value='1'>Special</option>
                  <option value='2'>Special (active)</option>
                  <option value='8'>Deadkey</option>
                  <option value='9'>Blank</option>
                  <option value='10'>Spacer</option>
                </select>
                <label for='selKeyLayerOverride'>Modifier:</label> <select id='selKeyLayerOverride'></select>
                <label for='inpKeyName'>Code:</label> <input id='inpKeyName' type='text' size='16' maxlength='64' />
                <input id='inpKeyCap' type='text' size='8' maxlength='16' />
                <label for='inpKeyPadding'>Padding Left:</label>
                <input id='inpKeyPadding' size='5' maxlength='5' />
                <label for='inpKeyWidth'>Width:</label>
                <input id='inpKeyWidth' size='5' maxlength='5' />
                <label for='selKeyNextLayer'>Next Layer:</label> <select id='selKeyNextLayer'></select>
                <button id='btnAddSubKeyArray'>Add longpress popup</button>
                <button id='btnDelSubKeyArray'>Remove longpress popup</button>
                <br class='clear' />
              </div>

              <div id="wedgeAddSubKeyLeft" class="skcontrol wedge-vert"><span>+</span></div>
              <div id="wedgeAddSubKeyRight" class="skcontrol wedge-vert"><span>+</span></div>
              <div id="btnDelSubKey">x</div>

              <div id='sk'></div>
                <div id='subKeyToolbar'>
                    <label for='selSubKeyType'>Key Type:</label> <select id='selSubKeyType'>
                        <option value='0'>Default</option>
                        <option value='1'>Special</option>
                        <option value='2'>Special (active)</option>
                        <option value='8'>Deadkey</option>
                        <option value='9'>Blank</option>
                        <option value='10'>Spacer</option>
                    </select>
                    <label for='selSubKeyLayerOverride'>Modifier:</label> <select id='selSubKeyLayerOverride'></select>
                    <label for='inpSubKeyName'>Code:</label> <input id='inpSubKeyName' type='text' size='16' maxlength='64' />
                    <input id='inpSubKeyCap' type='text' size='8' maxlength='16' />
                    <label for='selSubKeyNextLayer'>Next Layer:</label> <select id='selSubKeyNextLayer'></select>
                    <br class='clear' />
                </div>

                <div id='addPlatformDialog' title='Add platform'>
                    <form>
                        <fieldset>
                            Platform: <select id='selAddPlatform'></select>
                        </fieldset>
                    </form>
                </div>
                <div id='addLayerDialog' title='Add layer'>
                    <form>
                        <fieldset>
                            <label for='addLayerList'>Modifier-based layer:</label> <select id='addLayerList'></select><br />
                        </fieldset>
                        <fieldset>
                            <label for='addLayerName'>Name:</label> <input type='text' id='addLayerName' size='16' maxlength='64' /><br/>
                            <span id='addLayerNote'></span>
                        </fieldset>
                    </form>
                </div>
                <div id='selectKeyDialog' title='Select key'>
                  Press any key to select it on the keyboard
                </div>
                <div id='layerPropertiesDialog' title='Layer properties'>
                    <form>
                        <fieldset>
                            Name: <input type='text' id='layerName' size='16' maxlength='64' />
                        </fieldset>
                    </form>
                </div>
                <textarea id='data' cols='80' rows='30'></textarea>
            </body>
        </html>

    </xsl:template>
</xsl:stylesheet>
