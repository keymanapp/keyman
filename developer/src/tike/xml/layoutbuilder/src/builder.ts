/// <reference path="../../../../../../node_modules/@types/jquery/index.d.ts"/>
/// <reference path="../../../../../../node_modules/@types/jqueryui/index.d.ts"/>

// temporary interface to streamline transform to Typescript
interface Builder {
  [key: string]: any;
};

declare global {
  var KVKL: any;
};

export const builder: Builder = {
  filename: '',
  xscale: 1,
  yscale: 1,
  uniqId: 1,
  getPresentation: function () {
    return $('#selPlatformPresentation').val();
  },

  saveSelection: function() {
    let key = builder.selectedKey(), subKey = builder.selectedSubKey();
    return {
      id: key.length ? $(key).data('id') : null,
      subId: subKey.length ? $(subKey).data('id') : null
    };
  },

  restoreSelection: function(selection) {
    let key = $('#kbd .key').filter(function (_index, elem) { return $(elem).data('id') === selection.id; }).first();
    if(!key || !key.length) {
      let keys = $('#kbd div.key');
      if(!keys.length) return;
      key = $(keys[0]);
    }
    builder.selectKey(key);
    let subKey = $('#sub-key-groups .key').filter(function (_index, elem) { return $(elem).data('id') === selection.subId; }).first();
    if(!subKey || !subKey.length) {
      let subKeys = $('#sub-key-groups div.key');
      if(!subKeys.length) return;
      subKey = $(subKeys[0]);
    }
    builder.selectSubKey(subKey);
  },

  selPlatformPresentationChange: function () {
    let lastSelection = builder.saveSelection();
    builder.selectKey(null, false);
    builder.selectSubKey(null);
    builder.prepareLayer();
    builder.restoreSelection(lastSelection);
    builder.saveState();
  },

  removeAllSubKeys: function() {
    $('#sub-key-groups .key').remove();
  },

  prepareKeyCapTypes: function() {
    let types = $('#selKeyCapType'), subTypes = $('#selSubKeyCapType'), opts = '';
    for(let name of builder.specialKeyNames) {
      opts += '<option value="'+name+'">'+builder.renameSpecialKey(name)+' &nbsp; &nbsp; '+name+'</option>';
    }
    $(types).append(opts);
    $(subTypes).append(opts);
  },

  preparePlatforms: function () {
    var firstPlatform = null;
    $('#platforms').empty();
    for (var platform in KVKL) {
      firstPlatform = firstPlatform || platform;
      var ul = document.createElement('ul');
      var li = document.createElement('li');
      var a = document.createElement('a');
      $(a).data('platform', platform).text(platform).click(function (evt) {
        builder.selectPlatform($(evt.currentTarget).data('platform'));
      }).attr('href', 'javascript:void(0)');
      $(li).append(a).append(ul);
      $('#platforms').append(li);
      for (var layer in KVKL[platform].layer) {
        li = document.createElement('li');
        $(li).data('platform', platform).data('layer', layer).text(KVKL[platform].layer[layer].id).click(function (evt) {
          builder.selectPlatform($(evt.currentTarget).data('platform'));
          builder.selectLayer($(evt.currentTarget).data('layer'));
        });
        $(ul).append(li);
      }
    }

    $('#selPlatform option').remove();
    for (var platform in KVKL) {
      var opt = document.createElement('option');
      $(opt).append(platform);
      $('#selPlatform').append(opt);
    }

    builder.selectPlatform(firstPlatform);
  },

  /**
  * Replace default key names by special font codes for modifier keys (copied from kmwosk.js)
  *
  *  @param    {string}  oldText
  *  @return {string}
  **/

  renameSpecialKey: function (oldText) {
    //Note:  U+E000 *is* PUA but was not accepted by IE as a character in the EOT font, so Alt recoded as U+E019
    return builder.specialCharacters[oldText] ?
      String.fromCharCode(0xE000 + builder.specialCharacters[oldText]) :
      oldText;
  },

  prepareLayers: function () {
    builder.fillLayerSelect();
    builder.fillModifierSelect();
  },

  fillLayerSelect: function() {
    $('#selLayer option').remove();
    $('#selKeyNextLayer option').remove();
    $('#selSubKeyNextLayer option').remove();

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(none)');
    $('#selKeyNextLayer').append(opt);

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(none)');
    $('#selSubKeyNextLayer').append(opt);

    for (var layer in KVKL[builder.lastPlatform].layer) {
      var opt = document.createElement('option');
      $(opt).attr('value', layer)
            .append(KVKL[builder.lastPlatform].layer[layer].id);
      $('#selLayer').append(opt);

      opt = document.createElement('option');
      $(opt).append(KVKL[builder.lastPlatform].layer[layer].id);
      $('#selKeyNextLayer').append(opt);

      opt = document.createElement('option');
      $(opt).append(KVKL[builder.lastPlatform].layer[layer].id);
      $('#selSubKeyNextLayer').append(opt);
    }
  },

  fillModifierSelect: function() {
    var modifiers = builder.showAllModifierCombinations ? builder.validModifierCombinations : builder.minimalModifierCombinations;

    var
      $selKeyLayerOverride = $('#selKeyLayerOverride'),
      $selSubKeyLayerOverride = $('#selSubKeyLayerOverride'),
      $addLayerList = $('#addLayerList');

    var add = function(e, val, text?) {
      var opt = document.createElement('option');
      if(typeof text != 'undefined') {
        $(opt).attr('value', val);
        $(opt).text(text);
      } else {
        $(opt).text(val);
      }
      e.append(opt);
    };

    $('#selKeyLayerOverride option').remove();
    $('#selSubKeyLayerOverride option').remove();
    $('#addLayerList option').remove();

    add($selKeyLayerOverride, '', '(current layer)');
    add($selSubKeyLayerOverride, '', '(current layer)');
    add($addLayerList, '', '(custom)');


    for (var modifier = 0; modifier < modifiers.length; modifier++) {
      var name = builder.getModifierCombinationName(modifiers[modifier]);

      add($selKeyLayerOverride, name);
      add($selSubKeyLayerOverride, name);
      add($addLayerList, name);
    }

    var alreadyAdded = function(modifier) {
      return builder.minimalModifierCombinations.indexOf(modifier) >= 0;
    };


    // check all keys for modifier usage
    var modifierNames = [];

    for(var i = 0; i < KVKL[builder.lastPlatform].layer.length; i++) {
      var layer = KVKL[builder.lastPlatform].layer[i];
      for(var j = 0; j < layer.row.length; j++) {
        var row = layer.row[j];
        for(var k = 0; k < row.key.length; k++) {
          if(typeof row.key[k].layer != 'undefined' && modifierNames.indexOf(row.key[k].layer) < 0) {
            modifierNames.push(row.key[k].layer);
          }
          if(typeof row.key[k].sk != 'undefined') {
            for(var l = 0; l < row.key[k].sk.length; l++) {
              if(typeof row.key[k].sk[l].layer != 'undefined' && modifierNames.indexOf(row.key[k].sk[l].layer) < 0) {
                modifierNames.push(row.key[k].sk[l].layer);
              }
            }
          }
        }
      }
    }

    var isUsed = function(modifierName) {
      return modifierNames.indexOf(modifierName) >= 0;
    };

    if(!builder.showAllModifierCombinations) {
      // Add any layer names that are already referenced
      for(modifier = 0; modifier < builder.validModifierCombinations.length; modifier++) {
        var name = builder.getModifierCombinationName(builder.validModifierCombinations[modifier]);
        if(!alreadyAdded(builder.validModifierCombinations[modifier]) && isUsed(name)) {
          add($selKeyLayerOverride, name);
          add($selSubKeyLayerOverride, name);
        }
      }
    }
  },

  getModifierCombinationFromLayerId: function(id) {
    for(var i = 0; i < builder.validModifierCombinations.length; i++) {
      if(builder.getModifierCombinationName(builder.validModifierCombinations[i]) == id) {
        return builder.validModifierCombinations[i];
      }
    }
    return 0;
  },

  isLayerIdShifted: function(id) {
    return (builder.getModifierCombinationFromLayerId(id) & builder.modifierCodes.SHIFT) != 0;
  },

  /**
   * Adds annotating images to a key cap to indicate when it has sub keys
   */
  addKeyAnnotations: function(key) {
    const longpress = $(key).data('longpress'), flick = $(key).data('flick'), multitap = $(key).data('multitap'), hint = $(key).data('hint');

    $('.has-flick', key).remove();
    if (flick && flick.length) {
      for(let f of flick) {
        $(key).append('<div class="has-flick has-flick-'+f.direction+'"></div>');
      }
    }

    // We add longpress after flick so that the longpress icon can be shifted down if there is a visual conflict
    $('.has-longpress', key).remove();
    if (longpress && longpress.length) {
      $(key).append('<div class="has-longpress"></div>');
    }

    $('.has-multitap', key).remove();
    if (multitap && multitap.length) {
      $(key).append('<div class="has-multitap"></div>');
    }

    if(hint && hint.length) {
      $('.hint', key).addClass('custom-hint');
    } else {
      $('.hint', key).removeClass('custom-hint');
    }
  },

  hexToCodePoint: function(codePoint) {
    const codePointValue = parseInt(codePoint, 16);
    if (
      isNaN(codePointValue) ||
      codePointValue < 0 ||
      codePointValue > 0x10FFFF ||
      (0x0 <= codePointValue && codePointValue <= 0x1F) ||
      (0x80 <= codePointValue && codePointValue <= 0x9F)
    ) {
      return null;
    }
    return String.fromCodePoint(codePointValue);
  },

  unicodeKeyIdToString: function(id) {
    // duplicated from oskKey.ts
    if(!id || id.substr(0,2) != 'U_') {
      return null;
    }

    let result = '';
    const codePoints = id.substr(2).split('_');
    for(let codePoint of codePoints) {
      const codePointValue = builder.hexToCodePoint(codePoint);
      if(codePointValue) {
        result += codePointValue;
      }
    }
    return result ? result : null;
  },

  inferKeyText: function(text, id) {
    let val = typeof text == 'string' && text != '' ? text : builder.unicodeKeyIdToString(id) ?? '';
    return val;
  },

  inferKeyHintText: function(keyHint, longpress, flickArray, multitap) {
    let hint = keyHint;
    const flick = flickArray ? flickArray.reduce((o,f) => {o[f.direction] = f; return o}, {}) : null;
    if(!hint) {
      const source = KVKL[builder.lastPlatform].defaultHint ?? 'dot';
      switch(source) {
        case 'none':
          break;
        case 'dot':
          // Default for Keyman 15
          if(longpress && longpress.length) hint = '•';
          break;
        case 'longpress':
          if(longpress && longpress.length) hint = builder.inferKeyText(longpress[0].text, longpress[0].id);
          break;
        case 'multitap':
          if(multitap && multitap.length) hint = builder.inferKeyText(multitap[0].text, multitap[0].id);
          break;
        case 'flick':
          for(let direction of ['n','ne','e','se','s','sw','w','nw']) {
            if(flick && flick[direction]) {
              hint = builder.inferKeyText(flick[direction].text, flick[direction].id);
              break;
            }
          }
          break;
        case 'flick-n':
        case 'flick-ne':
        case 'flick-e':
        case 'flick-se':
        case 'flick-s':
        case 'flick-sw':
        case 'flick-w':
        case 'flick-nw':
          let direction = source.substr(6);
          if(flick && flick[direction]) hint = builder.inferKeyText(flick[direction].text, flick[direction].id);
          break;
      }
    }

    if(hint == null) return '';
    return builder.renameSpecialKey(hint);
  },

  escapeFontName: function(fontName) {
    if(!fontName) {
      return "";
    }
    return JSON.stringify(fontName);
  },

  prepareLayer: function () {
    var layer = KVKL[builder.lastPlatform].layer[builder.lastLayerIndex];

    var isLayerShifted = builder.isLayerIdShifted(layer.id);

    var width = 0;  // calculate the widest and rescale
    for (var i = 0; i < layer.row.length; i++) {
      var row = builder.addRow('bottom'), rowWidth = 0;
      for (var j = 0; j < layer.row[i].key.length; j++) {
        var key = layer.row[i].key[j];
        rowWidth += (key.width ? parseInt(key.width, 10) : 100) + (key.pad ? parseInt(key.pad, 10) : builder.keyMargin);
      }
      width = Math.max(width, rowWidth);
    }

    width += builder.keyMargin;   // add right hand margin
    var height = layer.row.length * (100 + builder.keyMargin);  // 50% of tablet height, 100 px per row, 5 px margin

    //
    // Scaling for different platform images
    //

    var pres = builder.presentations[builder.getPresentation()];
    if (pres) {
      builder.xscale = pres.x / width;
      builder.yscale = pres.y / height;
    } else {
      builder.xscale = 0.5;
      builder.yscale = 0.5;
    }

    $('#kbd,#sub-key-groups')
      .css('font-family', builder.escapeFontName(KVKL[builder.lastPlatform].font))
      .css('font-size', KVKL[builder.lastPlatform].fontsize || "1em");

    $('#inpKeyCap,#inpSubKeyCap')
      .css('font-family', builder.escapeFontName(KVKL[builder.lastPlatform].font))
      .css('font-size', '18pt');

    $('#kbd div').remove();
    for (var i = 0; i < layer.row.length; i++) {
      var row = builder.addRow('bottom');
      var calcKeyWidth = 0, calcGapWidth = 0;
      for (var j = 0; j < layer.row[i].key.length; j++) {
        var key = layer.row[i].key[j];
        var nkey = builder.addKey('key', row, key.sp);
        var w = key.width ? key.width : 100;
        var p = (key.pad ? key.pad : builder.keyMargin) * builder.xscale;
        let text = builder.inferKeyText(key.text, key.id);

        calcKeyWidth += parseInt(w, 10);
        calcGapWidth += parseInt(key.pad ? key.pad : builder.keyMargin, 10);

        $(nkey)
          .data('id', key.id)
          .data('pad', key.pad)
          .data('width', key.width)
          .data('sp', key.sp)
          .data('font', key.font)
          .data('fontsize', key.fontsize)
          .data('nextlayer', key.nextlayer)
          .data('layer', key.layer)
          .data('text', key.text)
          .data('hint', key.hint)
          .data('longpress', key.sk)
          .data('flick', builder.translateFlickObjectToArray(key.flick))
          .data('multitap', key.multitap)
          .data('type', 'key')

          .css('width', (w * builder.xscale) + 'px')
          .css('height', (100 * builder.yscale) + 'px')
          .css('margin-top', (builder.keyMargin * builder.yscale) + 'px')
          .css('margin-left', p + 'px')
          .css('font-family', builder.escapeFontName(key.font))
          .css('font-size', key.fontsize);

        if(builder.specialCharacters[text])
          $(nkey).addClass('key-special-text');

        builder.addKeyAnnotations(nkey);

        $('.text', nkey).text(builder.renameSpecialKey(text));
        builder.updateHint($(nkey));

        if(KVKL[builder.lastPlatform].displayUnderlying) $('.underlying', nkey).text(builder.getStandardKeyCap(key.id, key.layer ? builder.isLayerIdShifted(key.layer) : isLayerShifted));


        builder.updateKeyId(nkey);
      }

      $('#kbd').attr('class', builder.getPresentation());
    }

    builder.updateKeySizeInfo();
  },

  updateKeySizeInfo: function() {
    let rows = $('#kbd div.row');
    rows.each(function(_index, rowElem) {
      let keys = $('.key', rowElem), keyWidth = 0, gapWidth = 0;
      keys.each(function(_index, keyElem) {
        let width = $(keyElem).data('width'), pad = $(keyElem).data('pad');
        keyWidth += width ? parseInt(width, 10) : 100;
        gapWidth += pad ? parseInt(pad, 10) : builder.keyMargin;
      });
      let count = keys.length, totalWidth = keyWidth + gapWidth;
      $('.key-size', rowElem).html(count + ' keys<br>' +
        keyWidth + ' key width<br>' +
        gapWidth + ' padding<br>' +
        totalWidth + ' total');
    });
  },

  getStandardKeyCap: function (id, shifted) {
    id = id ? id.toUpperCase() : '';
    var i = builder.standardKeyNames.findIndex(function(x) { return x.toUpperCase() == id });
    return i >= 0 ? builder.standardKeyCaps[i][shifted ? 1 : 0] : '';
  },

  updateKeyId: function (nkey) {
    $('.id', nkey).text(($(nkey).data('layer') || '') + ' ' + $(nkey).data('id'));
  },

  selectPlatform: function (val) {
    builder.lastPlatform = val || $('#selPlatform').val();

    var listContainer = $('#selPlatformPresentation');
    $('option', listContainer).remove();

    for (var i in builder.presentations) {
      if (i.substring(0, builder.lastPlatform.length) != builder.lastPlatform) {
        continue;
      }
      var option = $(document.createElement('option'));
      option.attr('value', i).text(builder.presentations[i].name);
      listContainer.append(option);
    }

    builder.prepareLayers();
    builder.selectLayer(0);
  },

  selectLayer: function (val) {
    let selection = builder.saveSelection();
    if(val) $('#selLayer').val(val);
    builder.lastLayerIndex = $('#selLayer').val();
    builder.prepareLayer();
    builder.restoreSelection(selection);
  },

  selectLayerByName: function (name) {
    var layerOption = $('#selLayer option').filter(function (_index, elem) { return $(elem).text() === name; });
    if (layerOption.length == 0) {
      alert('Layer ' + name + ' not found.');
    } else {
      builder.selectLayer(layerOption.attr('value'));
    }
  },

  commands: [],

  command: function (cmd) {
    if (navigator.userAgent.indexOf('Keyman') < 0) {
      return false;
    }
    // queue commands to build a single portmanteau command when we return to idle
    if (builder.commands.length == 0) {
      window.setTimeout(function() {
        try {
          location.href = 'keyman:command?' + builder.commands.reduce(function(a,v) { return a + '&' + v; });
          builder.commands = [];
        } catch (e) {
          // ignore errors - saves weirdness happening when testing outside TIKE.exe
        }
      }, 10);
    }
    builder.commands.push(cmd);
    return true;
  },

  addRow: function (position) {
    var row = document.createElement('div');
    $(row).addClass('row');
    var parentRow = builder.selectedKey().parent();
    if (position == 'below') {
      $(parentRow).after(row);
    } else if (position == 'above') {
      $(parentRow).before(row);
    } else {
      $('#kbd').append(row);
    }

    var rowKeySize = document.createElement('div');
    $(rowKeySize).addClass('key-size');
    $(rowKeySize).text('15');
    $(row).append(rowKeySize);

    return row;
  },

  delSelectedKey: function () {
    var key = builder.selectedKey();
    var nextKey = $(key).next('.key');
    if (nextKey.length == 0) nextKey = $(key).prev('.key');
    if (nextKey.length == 0) {
      return builder.delSelectedRow();
    }
    $(key).remove();
    builder.selectKey(nextKey);
  },

  delSelectedRow: function () {
    var parentRow = builder.selectedKey().parent();
    var nextKey = $(parentRow).next().children('.key').first();
    if (nextKey.length == 0) {
      nextKey = $(parentRow).prev().children('.key').first();
    }
    if (nextKey.length == 0) {
      return;
    }
    builder.selectKey(nextKey);
    $(parentRow).remove();
  },

  selectKey: function (key, adjustResizable) {

    if (arguments.length == 1 || adjustResizable)
      builder.selectedKey().resizable('destroy');

    if(!builder.textControlsInToolbar()) {
      $('input#inpSubKeyCap').css('display', '');
    }
    $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,div#btnDelSubKey,input#inpSubKeyCap').css('display', '');
    builder.selectedKey().removeClass('selected');
    if (key && $(key).length) {
      $(key).addClass('selected');
      if(!builder.textControlsInToolbar()) {
        $('input#inpKeyCap').css('display', 'block');
      }
      $('.kcontrol.wedge-horz,.kcontrol.wedge-vert,div#btnDelKey').css('display', 'block');
      builder.moveWedgesAround(key);
      builder.prepareKey();
      let subKeys = $('#sub-key-groups div.key');
      if(subKeys.length) {
        builder.selectSubKey(subKeys[0]);
      }
    } else {
      if(!builder.textControlsInToolbar()) {
        $('input#inpKeyCap').css('display', '');
      }
      $('.kcontrol.wedge-horz,.kcontrol.wedge-vert,div#btnDelKey').css('display', '');
      builder.removeAllSubKeys();
      builder.enableKeyControls();
      builder.enableSubKeyControls();
    }
    builder.saveState();
  },

  moveWedgesAround: function(key) {
    const scrollOffset = $('#kbd-scroll-container').offset();
    const rowOffset = $(key).parent().offset();
    const offset = $(key).offset();
    const wedgeLeft = rowOffset.left < scrollOffset.left ? offset.left - 18 : rowOffset.left - 18;

    $('#wedgeAddRowAbove').offset({ left: wedgeLeft, top: rowOffset.top - 7 });
    $('#wedgeAddRowBelow').offset({ left: wedgeLeft, top: rowOffset.top + $(key).parent().outerHeight() - 7 });
    $('#wedgeAddKeyLeft').offset({ left: offset.left - 9, top: offset.top + $(key).outerHeight() + 2 });
    $('#wedgeAddKeyRight').offset({ left: offset.left + $(key).outerWidth() - 7, top: offset.top + $(key).outerHeight() + 2 });
    $('div#btnDelKey').offset({ left: offset.left + $(key).outerWidth() - 5, top: offset.top - 8 });
    if(!builder.textControlsInToolbar()) {
      $('input#inpKeyCap').offset({ left: offset.left + 16, top: offset.top + 4 }).width($(key).width() - 32);
    }
  },

  selectedKey: function () {
    return $('#kbd .selected');
  },

  wrapChange: function(f: (evt: JQueryEventObject) => any, opt) {
    return function(evt: JQueryEventObject) {
      if(typeof opt == 'object' && opt.saveOnce) {
        if (!builder.hasSavedKeyUndo) {
          builder.saveUndo();
          builder.hasSavedKeyUndo = true;
        } else {
          builder.command('modified');
        }
      } else {
        builder.saveUndo();
      }
      var r = f.apply(evt.currentTarget, [evt]);
      if(typeof opt == 'object' && opt.rescale) {
        builder.rescale();
      }
      builder.generate();
      return r;
    }
  },

  wrapInstant: function(f: (evt: JQueryEventObject) => any) {
    return function(evt: JQueryEventObject) {
      window.setTimeout(f.bind(evt.currentTarget, evt), 0);
    }
  },

  updateCharacterMap: function (val, fromSubKey) {
    // Update character map
    var src = $(fromSubKey ? '#inpSubKeyCap' : '#inpKeyCap')[0] as HTMLInputElement;

    var x1 = src.selectionStart;
    var x2 = src.selectionEnd;
    if (x2 != x1 || x1 > 0) {
      var ch = val.charCodeAt(x2 - 1);
      if (x2 > 1 && ch >= 0xDC00 && ch < 0xE000) {
        // yep, surrogate pairs again!
        var chHigh = val.charCodeAt(x2 - 2), chLow = ch;
        ch = ((chHigh - 0xD800) << 10) + (chLow - 0xDC00) + 0x10000;
      }
      builder.command('selected-char,' + ch.toString());
    }
  },

  toUnicodeString: function(s) {
    if(typeof s != 'string') return '';
    let r = '';
    for(let ch of s) {
      let code = ch.codePointAt(0).toString(16).toUpperCase();
      r += 'U+' + code.padStart(4, '0') + ' ';
    }
    return r.trim();
  },

  fromUnicodeString: function(s) {
    if(typeof s != 'string') return '';
    let chars = s.split(' '), r = '';
    for(let ch of chars) {
      if(!ch.match(/^u\+[0-9a-f]{1,6}$/i)) continue;
      const codePointValue = builder.hexToCodePoint(ch.substring(2));
      if(codePointValue) {
        r += codePointValue;
      }
    }
    return r;
  },

  keyCapChange: function(val) {
    const k = builder.selectedKey();
    k.data('text', val);
    let text = builder.inferKeyText(val, k.data('id'));
    $('.text', k).text(builder.renameSpecialKey(text));
    if(builder.specialCharacters[text]) {
      k.addClass('key-special-text');
    } else {
      k.removeClass('key-special-text');
    }
    builder.updateCharacterMap(text, false);
  },


  updateHint: function(key) {
    const val = key.data('hint');
    const hintElement = $('.hint', key);
    hintElement.text(builder.inferKeyHintText(val, $(key).data('longpress'), $(key).data('flick'), $(key).data('multitap')));
    if(val && val.length) {
      hintElement.addClass('custom-hint');
    } else {
      hintElement.removeClass('custom-hint');
    }
    if(builder.specialCharacters[val]) {
      hintElement.addClass('key-special-text');
    } else {
      hintElement.removeClass('key-special-text');
    }
  },

  keyHintChange: function(val) {
    const key = builder.selectedKey();
    key.data('hint', val);
    builder.updateHint(key);
  },

  enableKeyControls: function () {
    var key = builder.selectedKey();
    if (key.length == 0) {
      $('#keyToolbar *').attr('disabled', 'disabled');
      $('#sub-key-container').css('display', 'none');
      $('#key-cap-unicode-toolbar-item, #key-cap-toolbar-item').css('display', 'none');
    } else {
      let val = $(key).data('text');
      $('#keyToolbar *').removeAttr('disabled');
      $('#sub-key-container').css('display', '');
      $('#key-cap-unicode-toolbar-item, #key-cap-toolbar-item').css('display', builder.specialCharacters[val] ? 'none' : '');
    }
  },

  rescale: function () {
    builder.saveUndo();
    const k = builder.selectedKey();
    const keyId = k.data('id');
    const keyItems = $('#kbd .key').filter((_index,item) => $(item).data('id') === keyId);
    const keyItemIndex = keyItems.toArray().indexOf(k.length ? k[0] : null);
    builder.prepareLayer();
    if (keyId !== null && keyItemIndex >= 0) {
      const newKeyItems = $('#kbd .key').filter((_index,item) => $(item).data('id') === keyId);
      if(keyItemIndex < newKeyItems.length) {
        builder.selectKey(newKeyItems[keyItemIndex]);
      }
    }
  },

  translateFlickArrayToObject: function(flicks) {
    let res = {};
    for(let flick of flicks) {
      const {direction, ...item} = flick;
      res[direction] = item;
    }
    return res;
  },

  translateFlickObjectToArray: function(flicks) {
    if(!flicks) {
      return [];
    }

    return Object.getOwnPropertyNames(flicks).map(
      direction => {
        return {...flicks[direction], direction: direction};
      }
    );
  },

  generate: function (display, force) {
    var json = JSON.stringify(KVKL, null, '  ');

    var layer = KVKL[builder.lastPlatform].layer[builder.lastLayerIndex];
    layer.row = [];

    var rows = $('#kbd > div.row'), n = 1;
    $(rows).each(function (_index, rowElem) {
      var row = { id: n++, key: [] };
      $(rowElem).children(".key").each(function (_index, elem) {
        if ($(elem).hasClass('ui-draggable-dragging')) return;
        var key: any = { "id": $(elem).data('id'), "text": $(elem).data('text') };
        key.pad = $(elem).data('pad');
        key.width = $(elem).data('width');
        key.sp = $(elem).data('sp');
        key.font = $(elem).data('font');
        key.fontsize = $(elem).data('fontsize');
        key.nextlayer = $(elem).data('nextlayer');
        key.layer = $(elem).data('layer');
        key.hint = $(elem).data('hint');
        let longpress = $(elem).data('longpress');
        if(longpress && longpress.length) key.sk = longpress;
        let flick = $(elem).data('flick');
        if(flick && flick.length) key.flick = builder.translateFlickArrayToObject(flick);
        let multitap = $(elem).data('multitap');
        if(multitap && multitap.length) key.multitap = multitap;
        row.key.push(key);
      });
      layer.row.push(row);
    });

    builder.saveJSON(json, force);
  },

  saveJSON: function(force) {
    builder.cleanupKVKL();
    var newJson = JSON.stringify(KVKL, null, '  ');
    //TODO -- buggy if(force || newJson != json) {
      // When adding or deleting layers and platforms, we need to force because
      // that will not result in changes to the rows.
      $.ajax('/app/source/file', {
        'type': 'POST',
        'data': {
          'Filename': builder.filename,
          'Data': newJson
        }
      });
      $('#data').val(newJson);
    // }
  },

  ctrlDown: false,
  nextKeySelects: false,

  selectKeyByCode: function (code) {
    if (code >= 0 && code < 256) {
      var keyName = builder.standardKeyNames[code];
      var key = $('.key').filter(function (_index, elem) { return $(elem).data('id') === keyName; });
      if (key.length > 0) builder.selectKey(key[0]);
    }
  },

  loadingState: true,

  saveState: function() {
    if(builder.loadingState) return;

    var state = {
      platform: builder.lastPlatform,
      layer: builder.lastLayerIndex,
      presentation: $('#selPlatformPresentation').val(),
      key: undefined,
      subkey: undefined,
    };

    var key = builder.selectedKey();
    if(key.length > 0) {
      state.key = key.data('id');
    }

    var subkey = builder.selectedSubKey();
    if (subkey.length > 0) {
      state.subkey = $(subkey).data('id');
    }

    $.post('/app/source/toucheditor/state', {
      'Filename': builder.filename,
      'State': JSON.stringify(state)
    });
  },

  loadState: function() {
    $.get('/app/source/toucheditor/state',
      {
        'Filename': builder.filename
      },
      function(data) {
        builder.loadingState = false;
        if(typeof data === 'object') {
          if(data.platform && KVKL[data.platform]) {
            $('#selPlatform').val(data.platform);
          }
          builder.selectPlatform();
          if(data.presentation &&
              builder.presentations[data.presentation] &&
              $('#selPlatformPresentation option[value="'+data.presentation+'"]').length) {
            $('#selPlatformPresentation').val(data.presentation);
          } else {
            // The last selected presentation is no longer available; select the first option instead
            $('#selPlatformPresentation').val($('#selPlatformPresentation option:first').val());
          }

          let selection = builder.saveSelection();
          builder.prepareLayer();
          builder.restoreSelection(selection);

          if(data.layer && KVKL[builder.lastPlatform][data.layer]) {
            $('#selLayer').val(data.layer);
            builder.selectLayer();
          }
          if(data.key) {
            builder.selectKey($('#kbd .key').filter(function (_index, elem) { return $(elem).data('id') === data.key; }).first());
          }
          if(data.subkey) {
            builder.selectSubKey($('#sub-key-groups .key').filter(function (_index, elem) { return $(elem).data('id') === data.subkey; }).first());
          }
        }
      }
    );
  },

  textControlsInToolbar: function() {
    return $('body').hasClass('text-controls-in-toolbar');
  },
};

(function(search) {
  var q = search.match(/Filename=(.+)(&|$)/);
  if(q) {
    builder.filename = decodeURIComponent(q[1]);
  }
})(location.search);

$('#selPlatformPresentation').change(builder.selPlatformPresentationChange);

const inpKeyPaddingChange = builder.wrapChange(function (evt: JQueryEventObject) {
  builder.selectedKey().data('pad', $(evt.currentTarget).val())
                        .css('margin-left', $(evt.currentTarget).val() + 'px');
}, {rescale: true});

$('#inpKeyPadding')
  .change(inpKeyPaddingChange)
  .on('input', inpKeyPaddingChange);

const inpKeyWidthChange = builder.wrapChange(function (evt: JQueryEventObject) {
    builder.selectedKey().data('width', $(evt.currentTarget).val())
                         .css('width', parseInt($(evt.currentTarget).val(), 10) * builder.xscale + 'px');
  }, {rescale: true});

$('#inpKeyWidth')
  .change(inpKeyWidthChange)
  .on('input', inpKeyWidthChange);

const inpKeyNameChange = builder.wrapChange(function (evt: JQueryEventObject) {
  let key = builder.selectedKey();
  key.data('id', $(evt.currentTarget).val());
  builder.updateKeyId(key);
  builder.keyCapChange(key.data('text'));
}, {saveOnce: true});

$('#inpKeyName')
  .change(inpKeyNameChange)
  .autocomplete({
    source: builder.lookupKeyNames,
    change: inpKeyNameChange,
    select: builder.wrapInstant(inpKeyNameChange)
  })
  .on('input', inpKeyNameChange)
  .blur(function () {
    builder.hasSavedKeyUndo = false;
  });

const inpKeyCapChange = builder.wrapChange(function (evt: JQueryEventObject) {
  const val = $(evt.currentTarget).val();
  $('#inpKeyCapUnicode').val(builder.toUnicodeString(val));
  builder.keyCapChange(val);
}, {saveOnce: true});

const inpKeyCapUnicodeChange = builder.wrapChange(function (evt: JQueryEventObject) {
  const val = builder.fromUnicodeString($(evt.currentTarget).val());
  $('#inpKeyCap').val(val);
  builder.keyCapChange(val);
}, {saveOnce: true});

const inpKeyHintChange = builder.wrapChange(function (evt: JQueryEventObject) {
  const val = $(evt.currentTarget).val();
  $('#inpKeyHintUnicode').val(builder.toUnicodeString(val));
  builder.keyHintChange(val);
  builder.updateCharacterMap(val, false);
}, {saveOnce: true});

const inpKeyHintUnicodeChange = builder.wrapChange(function (evt: JQueryEventObject) {
  const val = builder.fromUnicodeString($(evt.currentTarget).val());
  $('#inpKeyHint').val(val);
  builder.keyHintChange(val);
  builder.updateCharacterMap(val, false);
}, {saveOnce: true});


const selKeyCapTypeChange = builder.wrapChange(function (evt: JQueryEventObject) {
  var val = $(evt.currentTarget).val();
  $('#inpKeyCap').val(val);
  $('#inpKeyCapUnicode').val(builder.toUnicodeString(val));
  builder.keyCapChange(val);
  // We only EnableControls here because if the user types *BkSp* into the
  // text field, we shouldn't hide the text field until next time the key is selected
  builder.enableKeyControls();
});

$('#selKeyCapType').on('change', selKeyCapTypeChange);

$('#inpKeyCap')
  .on('input', inpKeyCapChange)
  .change(inpKeyCapChange)
  .mouseup(function (evt) {
    builder.updateCharacterMap($(evt.currentTarget).val(), false);
  }).focus(function (evt) {
    builder.updateCharacterMap($(evt.currentTarget).val(), false);
  }).blur(function (_evt) {
    builder.hasSavedKeyUndo = false;
  });

$('#inpKeyCapUnicode')
  .on('input', inpKeyCapUnicodeChange)
  .change(inpKeyCapUnicodeChange)
  .mouseup(function (evt) {
    builder.updateCharacterMap(builder.fromUnicodeString($(evt.currentTarget).val()), false);
  }).focus(function (evt) {
    builder.updateCharacterMap(builder.fromUnicodeString($(evt.currentTarget).val()), false);
  }).blur(function (_evt) {
    builder.hasSavedKeyUndo = false;
  });

$('#inpKeyHint')
  .on('input', inpKeyHintChange)
  .change(inpKeyHintChange)
  .mouseup(function (evt) {
    builder.updateCharacterMap($(evt.currentTarget).val(), false);
  }).focus(function (evt) {
    builder.updateCharacterMap($(evt.currentTarget).val(), false);
  }).blur(function (_evt) {
    builder.hasSavedKeyUndo = false;
  });

$('#inpKeyHintUnicode')
  .on('input', inpKeyHintUnicodeChange)
  .change(inpKeyHintUnicodeChange)
  .mouseup(function (evt) {
    builder.updateCharacterMap(builder.fromUnicodeString($(evt.currentTarget).val()), false);
  }).focus(function (evt) {
    builder.updateCharacterMap(builder.fromUnicodeString($(evt.currentTarget).val()), false);
  }).blur(function (_evt) {
    builder.hasSavedKeyUndo = false;
  });

const selKeyTypeChange = builder.wrapChange(function (evt: JQueryEventObject) {
  var sp = $(evt.currentTarget).val();
  if (sp == 0) {
    builder.selectedKey().removeData('sp');
  } else {
    builder.selectedKey().data('sp', $(evt.currentTarget).val());
  }
  builder.formatKey(builder.selectedKey(), $(evt.currentTarget).val());
});

$('#selKeyType').change(selKeyTypeChange);

const selKeyNextLayerChange = builder.wrapChange(function (evt: JQueryEventObject) {
  $(evt.currentTarget).val() === '' ?
    builder.selectedKey().removeData('nextlayer') :
    builder.selectedKey().data('nextlayer', $(evt.currentTarget).val());
});

$('#selKeyNextLayer').change(selKeyNextLayerChange);

const selKeyLayerOverrideChange = builder.wrapChange(function (evt: JQueryEventObject) {
  $(evt.currentTarget).val() === '' ?
    builder.selectedKey().removeData('layer') :
    builder.selectedKey().data('layer', $(evt.currentTarget).val());
  builder.updateKeyId(builder.selectedKey());
});

$('#selKeyLayerOverride').change(selKeyLayerOverrideChange);

$('#kbd-scroll-container').on('scroll', function () {
  const key = builder.selectedKey();
  if(key.length) {
    builder.moveWedgesAround(key[0]);
  }
});
$('#wedgeAddRowAbove').click(builder.wrapChange(function () {
  var row = builder.addRow('above'); builder.selectKey(builder.addKey('key', row));
}, {rescale: true}));

$('#wedgeAddRowBelow').click(builder.wrapChange(function () {
  var row = builder.addRow('below'); builder.selectKey(builder.addKey('key', row));
}, {rescale: true}));

$('#wedgeAddKeyLeft').click(builder.wrapChange(function () {
  builder.selectKey(builder.addKey('key', 'before'));
}, {rescale: true}));

$('#wedgeAddKeyRight').click(builder.wrapChange(function () {
  builder.selectKey(builder.addKey('key', 'after'));
}, {rescale: true}));

$('#btnDelKey').click(builder.wrapChange(function () {
  builder.delSelectedKey();
}, {rescale: true}));

$('#btnGenerate').click(function () { builder.generate(false,false); });

$('input').focus(function (evt) {
  builder.lastFocus = evt.currentTarget;
});

$('#btnTemplate').click(function () {
  builder.command('template');
});

$('#btnImport').click(function () {
  builder.command('import');
});

$('#kbd').click(function () {
  builder.selectKey(null);
});

$(document).keydown(function (event) {
  if(!(event.originalEvent as KeyboardEvent).repeat) {
    // We will get this event repeatedly while ctrl is held down,
    // so if ctrlDown is cancelled, we don't want to set it again
    builder.ctrlDown = (event.which == 17);
  }
  if (builder.nextKeySelects) {
    $('#selectKeyDialog').dialog('close');
    event.preventDefault();
    event.stopImmediatePropagation();
    builder.nextKeySelects = false;
    builder.selectKeyByCode(event.which);
  }
});

$(document).keyup(function (event) {
  if (builder.ctrlDown && event.which == 17) {
    event.preventDefault();
    event.stopImmediatePropagation();
    builder.nextKeySelects = true;
    $('#selectKeyDialog').dialog('open');
  }
  builder.ctrlDown = false;
});

export function initBuilder() {
  $(function() {
    builder.cleanupKVKL();
    builder.prepareKeyCapTypes();
    builder.preparePlatforms();
    builder.enableUndoControls();
    builder.loadState();
  });
}