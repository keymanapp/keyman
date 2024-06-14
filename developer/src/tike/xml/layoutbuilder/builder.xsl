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
  <link rel="stylesheet" type='text/css'><xsl:attribute name="href"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-ui/jquery-ui.css</xsl:attribute></link>
  <link rel='stylesheet' type='text/css'><xsl:attribute name="href"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder.css</xsl:attribute></link>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-1.10.2.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>jquery-ui/jquery-ui.js</xsl:attribute></script>
  <script>
    var KVKL = <xsl:value-of select='/TouchLayoutBuilder/LayoutJS' />;
    window.builder = {};
  </script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>constants.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>undo.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>prepare-key.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>subkeys.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>view-controls.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>platform-controls.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>layer-controls.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder-charmap.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>drag-drop.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>cleanup.js</xsl:attribute></script>
  <script>initBuilder();</script>
</head>
<body class='text-controls-in-toolbar'>
  <div id='toolbar'>
    <div id='controlToolbar'>
      <div>
        <div class='toolbar-item toolbar-single-column'>
          <button id='btnTemplate'>Template...</button>
        </div>
        <div class='toolbar-item toolbar-single-column'>
          <button id='btnImport'>Import from On Screen</button>
        </div>
      </div>
    </div>
    <div id='layoutToolbar'>
      <div>
        <h3>View Controls</h3>
        <div class='toolbar-item toolbar-single-column'>
          <select id='selPlatformPresentation'></select>
        </div>
        <div class='toolbar-item toolbar-single-column'>
          <button id='btnViewOptions'>View options...</button>
        </div>
      </div>
      <div>
        <h3>Platform</h3>
        <div class='toolbar-item toolbar-2-small-buttons'>
          <select id='selPlatform'></select>
          <button id='btnAddPlatform' class='emoji-button'>➕</button>
          <button id='btnDelPlatform' class='emoji-button'>➖</button>
        </div>
        <div class='toolbar-item toolbar-single-column'>
          <button id='btnEditPlatform'>Edit...</button>
        </div>
      </div>
      <div>
        <h3>Layer</h3>
        <div class='toolbar-item toolbar-2-small-buttons'>
          <select id='selLayer'></select>
          <button id='btnAddLayer' class='emoji-button'>➕</button>
          <button id='btnDelLayer' class='emoji-button'>➖</button>
        </div>
        <div class='toolbar-item toolbar-single-column'>
          <button id='btnEditLayer'>Edit...</button>
        </div>
      </div>
    </div>
    <br class='clear' />
  </div>

  <div id='container'>

    <!-- Keyboard Container -->

    <div id='kbd-container'>
      <div id='kbd-scroll-container'>
        <div id='kbd'></div>
      </div>

      <!-- Keyboard Toolbar -->

      <div id='keyToolbar'>

        <div class='toolbar-item'>
          <label for='selKeyCapType'>Keycap Value:</label>
          <select id='selKeyCapType'>
            <option value=''>Text</option>
          </select>
        </div>

        <div class='toolbar-item' id='key-cap-toolbar-item'>
          <label for='inpKeyCap'>Text:</label>
          <input id='inpKeyCap' type='text' size='16' />
        </div>

        <div class='toolbar-item' id='key-cap-unicode-toolbar-item'>
          <label for='inpKeyCapUnicode'>Text Unicode:</label>
          <input id='inpKeyCapUnicode' type='text' size='16' />
        </div>

        <div class='toolbar-item' id='key-hint-toolbar-item'>
          <label for='inpKeyHint'>Hint:</label>
          <input id='inpKeyHint' type='text' size='16' />
        </div>

        <div class='toolbar-item' id='key-hint-unicode-toolbar-item'>
          <label for='inpKeyHintUnicode'>Hint Unicode:</label>
          <input id='inpKeyHintUnicode' type='text' size='16' />
        </div>

        <div class='toolbar-item'>
          <label for='selKeyType'>Key Type:</label>
          <select id='selKeyType'>
            <option value='0'>Default</option>
            <option value='1'>Special</option>
            <option value='2'>Special (active)</option>
            <option value='8'>Deadkey</option>
            <option value='9'>Blank</option>
            <option value='10'>Spacer</option>
          </select>
        </div>

        <div class='toolbar-item'>
          <label for='selKeyLayerOverride'>Modifier:</label>
          <select id='selKeyLayerOverride'></select>
        </div>

        <div class='toolbar-item'>
          <label for='inpKeyName'>ID:</label>
          <input id='inpKeyName' type='text' size='16' maxlength='64' />
        </div>

        <div class='toolbar-item'>
          <label for='inpKeyPadding'>Padding Left:</label>
          <input id='inpKeyPadding' size='5' maxlength='5' />
        </div>

        <div class='toolbar-item'>
          <label for='inpKeyWidth'>Width:</label>
          <input id='inpKeyWidth' size='5' maxlength='5' />
        </div>

        <div class='toolbar-item'>
          <label for='selKeyNextLayer'>Next Layer:</label>
          <select id='selKeyNextLayer'></select>
        </div>
      </div>

      <!-- Keyboard Controls -->

      <div id="wedgeAddRowAbove" class="kcontrol wedge-horz"><span>+</span></div>
      <div id="wedgeAddRowBelow" class="kcontrol wedge-horz"><span>+</span></div>
      <div id="wedgeAddKeyLeft" class="kcontrol wedge-vert"><span>+</span></div>
      <div id="wedgeAddKeyRight" class="kcontrol wedge-vert"><span>+</span></div>
      <div id="btnDelKey">x</div>
    </div>


    <!-- Longpress Container -->

    <div id='sub-key-container'>
      <div id='sub-key-groups'>
        <div>
          <span class='tab' id='tab-longpress'>Longpress Keys</span>

          <div id='longpress'>
            <div class='key-placeholder' id='btnAddLongpress'>+</div>
          </div>
        </div>

        <div>
          <span class='tab' id='tab-flick'>Flicks</span>
          <div id='flick'>
            <div class='key-placeholder btn-add-flick' id='flick-nw'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-n'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-ne'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-w'>+</div>
            <div class='key-placeholder' id='flick-center'></div>
            <div class='key-placeholder btn-add-flick' id='flick-e'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-sw'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-s'>+</div>
            <div class='key-placeholder btn-add-flick' id='flick-se'>+</div>
          </div>
        </div>

        <div>
          <span class='tab' id='tab-multitap'>Multitaps</span>
          <div id='multitap'>
            <div class='key-placeholder' id='btnAddMultitap'>+</div>
          </div>
        </div>
      </div>

      <div id='subKeyToolbar'>
        <div class='toolbar-item'>
          <label for='inpSubKeyGestureType'>Gesture Type:</label>
          <input id='inpSubKeyGestureType' type='text' size='16' disabled='disabled' />
        </div>
        <div class='toolbar-item'>
          <label for='selSubKeyCapType'>Keycap Value:</label>
          <select id='selSubKeyCapType'>
            <option value=''>Text</option>
          </select>
        </div>
        <div class='toolbar-item' id='sub-key-cap-toolbar-item'>
          <label>Text:</label>
          <input id='inpSubKeyCap' type='text' size='8' />
        </div>
        <div class='toolbar-item' id='sub-key-cap-unicode-toolbar-item'>
          <label>Text Unicode:</label>
          <input id='inpSubKeyCapUnicode' type='text' size='16' />
        </div>
        <div class='toolbar-item'>
          <label for='selSubKeyType'>Key Type:</label>
          <select id='selSubKeyType'>
            <option value='0'>Default</option>
            <option value='1'>Special</option>
            <option value='2'>Special (active)</option>
            <option value='8'>Deadkey</option>
            <option value='9'>Blank</option>
            <option value='10'>Spacer</option>
          </select>
        </div>
        <div class='toolbar-item'>
          <label for='selSubKeyLayerOverride'>Modifier:</label>
          <select id='selSubKeyLayerOverride'></select>
        </div>
        <div class='toolbar-item'>
          <label for='inpSubKeyName'>ID:</label>
          <input id='inpSubKeyName' type='text' size='16' maxlength='64' />
        </div>
        <div class='toolbar-item'>
          <label for='selSubKeyNextLayer'>Next Layer:</label>
          <select id='selSubKeyNextLayer'></select>
        </div>
        <div class='toolbar-item'>
          <label for='chkSubKeyIsDefault'>Default selection:</label>
          <input id='chkSubKeyIsDefault' type='checkbox' />
        </div>
      </div>

      <!-- Subkey Controls -->

      <div id="wedgeAddSubKeyLeft" class="skcontrol wedge-vert"><span>+</span></div>
      <div id="wedgeAddSubKeyRight" class="skcontrol wedge-vert"><span>+</span></div>
      <div id="btnDelSubKey">x</div>

    </div>

  </div>

  <div id='addPlatformDialog' title='Add platform'>
    <form>
      <fieldset>
        Platform: <select id='selAddPlatform'></select>
      </fieldset>
    </form>
  </div>

  <div id='addPlatformDialogNoPlatformsToAdd' title='Add platform'>
    <p>All available platforms have already been added.</p>
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

  <div id='viewOptionsDialog' title='View options'>
    <form>
      <fieldset>
        <div>By default, only common modifiers are shown in the modifier lists. If
          access to more unusual modifier combinations are required, check this option.<br/><br/>
          This option only affects the controls within the Touch Layout Editor; it has no impact on the layout itself.<br/><br/>
        </div>
        <input type='checkbox' id='chkShowAllModifierOptions'/>
        <label for='chkShowAllModifierOptions'>Show all modifier options</label>
      </fieldset>
    </form>
  </div>

  <div id='platformPropertiesDialog' title='Platform properties'>
    <form>
      <fieldset>
        <div>Displays the underlying keyboard label on top-left of the key<br/><br/></div>
        <input type='checkbox' id='chkDisplayUnderlying'/>
        <label for='chkDisplayUnderlying'>Display underlying key label</label>
      </fieldset>
      <fieldset>
        <div>Default hint source for keys for this platform. These can be
          overridden on a key-by-key basis. "Show a dot if a longpress is present"
          is the behavior in Keyman 15 and earlier versions and is the default option.<br/><br/>
        </div>
        <label for='selDefaultHint'>Default hint source:</label>
        <select id='selDefaultHint'>
          <option value='none'>No default hint</option>
          <option value='dot'>Show a dot if a longpress is present</option>
          <option value='longpress'>Use first longpress key cap</option>
          <option value='multitap'>Use first multitap key cap</option>
          <option value='flick'>Use first flick key cap (priority: n,ne,e,se,s,sw,w,nw)</option>
          <option value='flick-n'>Use flick north key cap</option>
          <option value='flick-ne'>Use flick north-east key cap</option>
          <option value='flick-e'>Use flick east key cap</option>
          <option value='flick-se'>Use flick south-east key cap</option>
          <option value='flick-s'>Use flick south key cap</option>
          <option value='flick-sw'>Use flick south-west key cap</option>
          <option value='flick-w'>Use flick west key cap</option>
          <option value='flick-nw'>Use flick north-west key cap</option>
        </select>
      </fieldset>
    </form>
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
