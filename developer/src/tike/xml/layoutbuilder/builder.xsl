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
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>platform-controls.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>layer-controls.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>builder-charmap.js</xsl:attribute></script>
  <script><xsl:attribute name="src"><xsl:value-of select="/TouchLayoutBuilder/LibPath"/>drag-drop.js</xsl:attribute></script>
  <script>initBuilder();</script>
</head>
<body class='text-controls-in-toolbar'>

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
        <label for='inpKeyCapUnicode'>Unicode:</label>
        <input id='inpKeyCapUnicode' type='text' size='16' />
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
        <label>Unicode:</label>
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
    </div>

    <!-- Subkey Controls -->

    <div id="wedgeAddSubKeyLeft" class="skcontrol wedge-vert"><span>+</span></div>
    <div id="wedgeAddSubKeyRight" class="skcontrol wedge-vert"><span>+</span></div>
    <div id="btnDelSubKey">x</div>

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
