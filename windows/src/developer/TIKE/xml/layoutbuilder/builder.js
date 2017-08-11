$(function () {
  window.builder = this;

  this.undoStack = [];
  this.redoStack = [];
  this.xscale = 1;
  this.yscale = 1;
  this.uniqId = 1;

  this.standardKeyNames = [
	'K_?00', 			// &H0
	'K_LBUTTON', 		// &H1
	'K_RBUTTON', 		// &H2
	'K_CANCEL', 	   	// &H3
	'K_MBUTTON', 		// &H4
	'K_?05', 			// &H5
	'K_?06', 			// &H6
	'K_?07', 			// &H7
	'K_BKSP',     		// &H8
	'K_TAB',     		// &H9
	'K_?0A', 			// &HA
	'K_?0B', 			// &HB
	'K_KP5', 	    	// &HC
	'K_ENTER', 			// &HD
	'K_?0E', 			// &HE
	'K_?0F', 			// &HF
	'K_SHIFT', 			// &H10
	'K_CONTROL', 		// &H11
	'K_ALT', 			// &H12
	'K_PAUSE', 			// &H13
	'K_CAPS', 			// &H14
	'K_KANJI?15', 		// &H15
	'K_KANJI?16', 		// &H16
	'K_KANJI?17', 		// &H17
	'K_KANJI?18', 		// &H18
	'K_KANJI?19', 		// &H19
	'K_?1A', 			// &H1A
	'K_ESC', 			// &H1B
	'K_KANJI?1C', 		// &H1C
	'K_KANJI?1D', 		// &H1D
	'K_KANJI?1E', 		// &H1E
	'K_KANJI?1F', 		// &H1F
	'K_SPACE', 			// &H20
	'K_PGUP', 			// &H21
	'K_PGDN', 			// &H22
	'K_END', 			// &H23
	'K_HOME', 			// &H24
	'K_LEFT', 			// &H25
	'K_UP', 			// &H26
	'K_RIGHT', 			// &H27
	'K_DOWN', 			// &H28
	'K_SEL', 			// &H29
	'K_PRINT', 			// &H2A
	'K_EXEC', 			// &H2B
	'K_PRTSCN', 		// &H2C
	'K_INS', 			// &H2D
	'K_DEL', 			// &H2E
	'K_HELP', 			// &H2F
	'K_0', 				// &H30
	'K_1', 				// &H31
	'K_2', 				// &H32
	'K_3', 				// &H33
	'K_4', 				// &H34
	'K_5', 				// &H35
	'K_6', 				// &H36
	'K_7', 				// &H37
	'K_8', 				// &H38
	'K_9', 				// &H39
	'K_?3A', 			// &H3A
	'K_?3B', 			// &H3B
	'K_?3C', 			// &H3C
	'K_?3D', 			// &H3D
	'K_?3E', 			// &H3E
	'K_?3F', 			// &H3F
	'K_?40', 			// &H40

	'K_A', 				// &H41
	'K_B', 				// &H42
	'K_C', 				// &H43
	'K_D', 				// &H44
	'K_E', 				// &H45
	'K_F', 				// &H46
	'K_G', 				// &H47
	'K_H', 				// &H48
	'K_I', 				// &H49
	'K_J', 				// &H4A
	'K_K', 				// &H4B
	'K_L', 				// &H4C
	'K_M', 				// &H4D
	'K_N', 				// &H4E
	'K_O', 				// &H4F
	'K_P', 				// &H50
	'K_Q', 				// &H51
	'K_R', 				// &H52
	'K_S', 				// &H53
	'K_T', 				// &H54
	'K_U', 				// &H55
	'K_V', 				// &H56
	'K_W', 				// &H57
	'K_X', 				// &H58
	'K_Y', 				// &H59
	'K_Z', 				// &H5A
	'K_?5B', 			// &H5B
	'K_?5C', 			// &H5C
	'K_?5D', 			// &H5D
	'K_?5E', 			// &H5E
	'K_?5F', 			// &H5F
	'K_NP0', 			// &H60
	'K_NP1', 			// &H61
	'K_NP2', 			// &H62
	'K_NP3', 			// &H63
	'K_NP4', 			// &H64
	'K_NP5', 			// &H65
	'K_NP6', 			// &H66
	'K_NP7', 			// &H67
	'K_NP8', 			// &H68
	'K_NP9', 			// &H69
	'K_NPSTAR', 		// &H6A
	'K_NPPLUS', 		// &H6B
	'K_SEPARATOR', 		// &H6C
	'K_NPMINUS', 		// &H6D
	'K_NPDOT', 			// &H6E
	'K_NPSLASH', 		// &H6F
	'K_F1', 			// &H70
	'K_F2', 			// &H71
	'K_F3', 			// &H72
	'K_F4', 			// &H73
	'K_F5', 			// &H74
	'K_F6', 			// &H75
	'K_F7', 			// &H76
	'K_F8', 			// &H77
	'K_F9', 			// &H78
	'K_F10', 			// &H79
	'K_F11', 			// &H7A
	'K_F12', 			// &H7B
	'K_F13', 			// &H7C
	'K_F14', 			// &H7D
	'K_F15', 			// &H7E
	'K_F16', 			// &H7F
	'K_F17', 			// &H80
	'K_F18', 			// &H81
	'K_F19', 			// &H82
	'K_F20', 			// &H83
	'K_F21', 			// &H84
	'K_F22', 			// &H85
	'K_F23', 			// &H86
	'K_F24', 			// &H87

	'K_?88', 			// &H88
	'K_?89', 			// &H89
	'K_?8A', 			// &H8A
	'K_?8B', 			// &H8B
	'K_?8C', 			// &H8C
	'K_?8D', 			// &H8D
	'K_?8E', 			// &H8E
	'K_?8F', 			// &H8F

	'K_NUMLOCK', 		// &H90
	'K_SCROLL', 		// &H91

	'K_?92', 			// &H92
	'K_?93', 			// &H93
	'K_?94', 			// &H94
	'K_?95', 			// &H95
	'K_?96', 			// &H96
	'K_?97', 			// &H97
	'K_?98', 			// &H98
	'K_?99', 			// &H99
	'K_?9A', 			// &H9A
	'K_?9B', 			// &H9B
	'K_?9C', 			// &H9C
	'K_?9D', 			// &H9D
	'K_?9E', 			// &H9E
	'K_?9F', 			// &H9F
	'K_?A0', 			// &HA0
	'K_?A1', 			// &HA1
	'K_?A2', 			// &HA2
	'K_?A3', 			// &HA3
	'K_?A4', 			// &HA4
	'K_?A5', 			// &HA5
	'K_?A6', 			// &HA6
	'K_?A7', 			// &HA7
	'K_?A8', 			// &HA8
	'K_?A9', 			// &HA9
	'K_?AA', 			// &HAA
	'K_?AB', 			// &HAB
	'K_?AC', 			// &HAC
	'K_?AD', 			// &HAD
	'K_?AE', 			// &HAE
	'K_?AF', 			// &HAF
	'K_?B0', 			// &HB0
	'K_?B1', 			// &HB1
	'K_?B2', 			// &HB2
	'K_?B3', 			// &HB3
	'K_?B4', 			// &HB4
	'K_?B5', 			// &HB5
	'K_?B6', 			// &HB6
	'K_?B7', 			// &HB7
	'K_?B8', 			// &HB8
	'K_?B9', 			// &HB9

	'K_COLON', 			// &HBA
	'K_EQUAL', 			// &HBB
	'K_COMMA', 			// &HBC
	'K_HYPHEN', 		// &HBD
	'K_PERIOD', 		// &HBE
	'K_SLASH', 			// &HBF
	'K_BKQUOTE', 		// &HC0

	'K_?C1', 			// &HC1
	'K_?C2', 			// &HC2
	'K_?C3', 			// &HC3
	'K_?C4', 			// &HC4
	'K_?C5', 			// &HC5
	'K_?C6', 			// &HC6
	'K_?C7', 			// &HC7
	'K_?C8', 			// &HC8
	'K_?C9', 			// &HC9
	'K_?CA', 			// &HCA
	'K_?CB', 			// &HCB
	'K_?CC', 			// &HCC
	'K_?CD', 			// &HCD
	'K_?CE', 			// &HCE
	'K_?CF', 			// &HCF
	'K_?D0', 			// &HD0
	'K_?D1', 			// &HD1
	'K_?D2', 			// &HD2
	'K_?D3', 			// &HD3
	'K_?D4', 			// &HD4
	'K_?D5', 			// &HD5
	'K_?D6', 			// &HD6
	'K_?D7', 			// &HD7
	'K_?D8', 			// &HD8
	'K_?D9', 			// &HD9
	'K_?DA', 			// &HDA

	'K_LBRKT', 			// &HDB
	'K_BKSLASH', 		// &HDC
	'K_RBRKT', 			// &HDD
	'K_QUOTE', 			// &HDE
	'K_oDF', 			// &HDF
	'K_oE0', 			// &HE0
	'K_oE1', 			// &HE1
	'K_oE2', 			// &HE2
	'K_oE3', 			// &HE3
	'K_oE4', 			// &HE4

	'K_?E5', 			// &HE5

	'K_oE6', 			// &HE6

	'K_?E7', 			// &HE7
	'K_?E8', 			// &HE8

	'K_oE9', 			// &HE9
	'K_oEA', 			// &HEA
	'K_oEB', 			// &HEB
	'K_oEC', 			// &HEC
	'K_oED', 			// &HED
	'K_oEE', 			// &HEE
	'K_oEF', 			// &HEF
	'K_oF0', 			// &HF0
	'K_oF1', 			// &HF1
	'K_oF2', 			// &HF2
	'K_oF3', 			// &HF3
	'K_oF4', 			// &HF4
	'K_oF5', 			// &HF5

	'K_?F6', 			// &HF6
	'K_?F7', 			// &HF7
	'K_?F8', 			// &HF8
	'K_?F9', 			// &HF9
	'K_?FA', 			// &HFA
	'K_?FB', 			// &HFB
	'K_?FC', 			// &HFC
	'K_?FD', 			// &HFD
	'K_?FE', 			// &HFE
	'K_?FF'  				// &HFF
  ];

  this.lookupKeyNames = [];

  for (var i = 0; i < this.standardKeyNames.length; i++) {
    if (this.standardKeyNames[i].indexOf('?') < 0) {
      this.lookupKeyNames.push(this.standardKeyNames[i]);
    }
  }
  this.lookupKeyNames.sort();

  this.specialKeyNames = ['*Shift*', '*Enter*', '*Tab*', '*BkSp*', '*Menu*', '*Hide*', '*Alt*', '*Ctrl*', '*Caps*',
      '*ABC*', '*abc*', '*123*', '*Symbol*', '*Currency*', '*Shifted*', '*AltGr*', '*TabLeft*'];

  this.presentations = {
    "tablet-ipad-landscape": { "x": 829, "y": 299, "name": "iPad (landscape)" }, // 829x622 = iPad tablet box size; (97,101)-(926,723)
    "tablet-ipad-portrait": { "x": 605, "y": 300, "name": "iPad (portrait)" }, // 605x806 = iPad tablet box size; (98,94)-(703,900)
    "phone-iphone5-landscape": { "x": 731, "y": 196, "name": "iPhone 5 (landscape)" }, // 731x412 = iPhone box size; (144,39)-(875,451)
    "phone-iphone5-portrait": { "x": 526, "y": 266, "name": "iPhone 5 (portrait)"}  // 528x936 = iPhone box size; (90,204)-(618,1040)
  };

  this.keyMargin = 5;

  this.layerOverrides = ['default', 'shift', 'ctrl', 'ctrlshift', 'alt', 'altshift', 'ctrlalt', 'ctrlaltshift'];

  this.getPresentation = function () {
    var platform = $('#selPlatformPresentation').val();
    //if(platform == 'tablet') return 'tablet-ipad';
    //if(platform == 'phone') return 'phone-iphone5';
    return platform;
  }

  $('#selPlatformPresentation').change(function () {
    builder.selectKey(null, false);
    builder.selectSubKey(null);
    builder.prepareLayer();
  });

  this.preparePlatforms = function () {
    var firstPlatform = null;
    $('#platforms').empty();
    for (var platform in KVKL) {
      firstPlatform = firstPlatform || platform;
      var ul = document.createElement('ul');
      var li = document.createElement('li');
      var a = document.createElement('a');
      $(a).data('platform', platform).text(platform).click(function () {
        builder.selectPlatform($(this).data('platform'));
      }).attr('href', 'javascript:void(0)');
      $(li).append(a).append(ul);
      $('#platforms').append(li);
      for (var layer in KVKL[platform].layer) {
        li = document.createElement('li');
        $(li).data('platform', platform).data('layer', layer).text(KVKL[platform].layer[layer].id).click(function () {
          builder.selectPlatform($(this).data('platform'));
          builder.selectLayer($(this).data('layer'));
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
  }

  /**
  * Replace default key names by special font codes for modifier keys (copied from kmwosk.js)
  *
  *  @param    {string}  oldText    
  *  @return {string}      
  **/

  this.renameSpecialKey = function (oldText) {
    var specialText = this.specialKeyNames;
    var codePUA = [8, 5, 6, 4, 11, 10, 25, 1, 3, 16, 17, 19, 21, 20, 8, 2, 7]; // set SHIFTED->9 for filled arrow icon

    //Note:  U+E000 *is* PUA but was not accepted by IE as a character in the EOT font, so Alt recoded as U+E019
    for (var i = 0; i < specialText.length; i++) {
      if (oldText == specialText[i]) {
        return String.fromCharCode(0xE000 + codePUA[i]);
      }
    }
    return oldText;
  }

  this.prepareLayers = function () {
    $('#selLayer option').remove();
    $('#selKeyNextLayer option').remove();
    $('#selSubKeyNextLayer option').remove();
    $('#selKeyLayerOverride option').remove();
    $('#selSubKeyLayerOverride option').remove();

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(none)');
    $('#selKeyNextLayer').append(opt);

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(none)');
    $('#selSubKeyNextLayer').append(opt);

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(layer default)');
    $('#selKeyLayerOverride').append(opt);

    opt = document.createElement('option');
    $(opt).attr('value', '').text('(layer default)');
    $('#selSubKeyLayerOverride').append(opt);

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
    for (var layer = 0; layer < this.layerOverrides.length; layer++) {
      opt = document.createElement('option');
      $(opt).append(this.layerOverrides[layer]);
      $('#selKeyLayerOverride').append(opt);

      opt = document.createElement('option');
      $(opt).append(this.layerOverrides[layer]);
      $('#selSubKeyLayerOverride').append(opt);
    }
  }

  this.prepareLayer = function () {
    var layer = KVKL[builder.lastPlatform].layer[builder.lastLayerIndex];

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

    var pres = this.presentations[builder.getPresentation()];
    if (pres) {
      this.xscale = pres.x / width;
      this.yscale = pres.y / height;
    } else {
      this.xscale = 0.5;
      this.yscale = 0.5;
    }

    $('#kbd,#sk')
      .css('font-family', KVKL[builder.lastPlatform].font)
      .css('font-size', KVKL[builder.lastPlatform].fontsize || "1em");

    $('#inpKeyCap,#inpSubKeyCap')
      .css('font-family', KVKL[builder.lastPlatform].font)
      .css('font-size', '18pt');

    $('#kbd div').remove();
    for (var i = 0; i < layer.row.length; i++) {
      var row = builder.addRow('bottom');
      var calcKeyWidth = 0, calcGapWidth = 0;
      for (var j = 0; j < layer.row[i].key.length; j++) {
        var key = layer.row[i].key[j];
        var nkey = builder.addKey(row, false, key.sp);
        var w = key.width ? key.width : 100;
        var p = (key.pad ? key.pad : builder.keyMargin) * this.xscale;
        if (key.sk) $(nkey).addClass('hasSubKeyArray');
        var code = key.id ? String.fromCharCode(parseInt(key.id.substring(2), 16)) : 0;
        var text = typeof key.text == 'string' ? key.text : (code == '' || code.charCodeAt(0) < 32 ? '' : code);

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
          .data('text', text)
          .data('sk', key.sk)

          .css('width', (w * this.xscale) + 'px')
          .css('height', (100 * this.yscale) + 'px')
          .css('margin-top', (builder.keyMargin * this.yscale) + 'px')
          .css('margin-left', p + 'px')
          .css('font-family', key.font)
          .css('font-size', key.fontsize);
        $('.text', nkey).text(this.renameSpecialKey(text));

        builder.updateKeyId(nkey);
      }

      var calcWidth = calcKeyWidth + calcGapWidth;

      $('.key-size', row).html(layer.row[i].key.length + ' keys<br>' +
         calcKeyWidth + ' key width<br>' +
         calcGapWidth + ' padding<br>' +
         calcWidth + ' total');

      $('#kbd').attr('class', builder.getPresentation()); //builder.lastPlatform); //css('background-position-y', '-200px');
    }

    var div = document.createElement('div');
    $(div).css('clear', 'both');
    $('#kbd').append(div);
  };

  this.updateKeyId = function (nkey) {
    $('.id', nkey).text(($(nkey).data('layer') || '') + ' ' + $(nkey).data('id'));
  }

  this.selectPlatform = function (val) {
    builder.lastPlatform = val || $('#selPlatform').val();

    var listContainer = $('#selPlatformPresentation');
    $('option', listContainer).remove();

    for (var i in this.presentations) {
      if (i.substring(0, builder.lastPlatform.length) != builder.lastPlatform) {
        continue;
      }
      var option = $(document.createElement('option'));
      option.attr('value', i).text(this.presentations[i].name);
      listContainer.append(option);
    }

    builder.prepareLayers();
    builder.selectLayer(0);
  }

  this.selectLayer = function (val) {
    builder.lastLayerIndex = val || $('#selLayer').val();
    builder.prepareLayer();
    builder.selectKey($('#kbd > div.row > div.key')[0]);
  }

  this.selectLayerByName = function (name) {
    var layerOption = $('#selLayer option').filter(function (index) { return $(this).text() === name; });
    if (layerOption.length == 0) {
      alert('Layer ' + name + ' not found.');
    } else {
      builder.selectLayer(layerOption.attr('value'));
    }
  }

  this.undo = function () {
    if (builder.undoStack.length == 0) {
      return;
    }
    builder.saveUndo(1);
    var s = builder.undoStack.pop();
    this.loadUndo(s);
  }

  this.redo = function () {
    if (builder.redoStack.length == 0) {
      return;
    }
    builder.saveUndo(2);
    var s = builder.redoStack.pop();
    this.loadUndo(s);
  }

  this.loadUndo = function (s) {
    KVKL = JSON.parse(s.KVKL);

    builder.preparePlatforms();
    builder.enableUndoControls();

    $('#selPlatform').val(s.platform);
    builder.selectPlatform();
    $('#selLayer').val(s.layer);
    builder.selectLayer();
    builder.selectKey($('#kbd .key').filter(function (index) { return $(this).data('id') === s.key; }));
    if (s.subkey) builder.selectSubKey($('#sk .key').filter(function (index) { return $(this).data('id') === s.subkey; }));
  }

  this.saveUndo = function (saveToRedo) {
    if (!saveToRedo) {
      builder.redoStack = [];
    }
    builder.generate(true);
    var s = {
      KVKL: JSON.stringify(KVKL),
      platform: builder.lastPlatform,
      layer: builder.lastLayerIndex,
      key: builder.selectedKey().data('id')
    };
    var key = builder.selectedSubKey();
    if (key.length > 0) {
      s.subkey = $(key).data('id');
    }

    var stack = (saveToRedo == 1 ? builder.redoStack : builder.undoStack);
    stack.push(s);
    if (stack.length > 100) {
      stack.shift();
    }
    builder.enableUndoControls();
    builder.command('modified');
  }

  this.command = function (cmd) {
    try {
      if (navigator.userAgent.indexOf('TIKE') >= 0) {
        location.href = 'keyman:' + cmd;
      }
    } catch (e) {
      // ignore errors - saves weirdness happening when testing outside TIKE.exe
    }
  }

  this.enableUndoControls = function () {
    if (builder.undoStack.length == 0) {
      builder.command('undo-disable');
      $('#btnUndo').attr('disabled', 'disabled');
    } else {
      builder.command('undo-enable');
      $('#btnUndo').removeAttr('disabled');
    }
    if (builder.redoStack.length == 0) {
      builder.command('redo-disable');
      $('#btnRedo').attr('disabled', 'disabled');
    } else {
      builder.command('redo-enable');
      $('#btnRedo').removeAttr('disabled');
    }
  }

  this.addRow = function (position) {
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
  };

  this.addKey = function (position, isSubKey, sp) {
    var key = document.createElement('div');
    var ktext = document.createElement('div');
    var kid = document.createElement('div');
    $(kid).addClass('id');
    $(ktext).addClass('text');
    $(key).append(kid);
    $(key).append(ktext);
    $(key).addClass('key');
    $(key).data('id', 'T_new_' + this.uniqId);
    this.uniqId++;
    builder.updateKeyId(key);

    if (isSubKey) {
      $(key).click(function (event) {
        event.stopPropagation();
        builder.selectSubKey(this);
        if (builder.lastFocus) {
          $(builder.lastFocus).focus().select();
        }
      }).dblclick(function (event) {
        event.stopPropagation();
        var layer = $(this).data('nextlayer');
        if (layer) builder.selectLayerByName(layer);
      });
      $(kid).click(function (event) {
        event.stopPropagation();
        builder.selectSubKey(key);
        $('#inpSubKeyName').focus().select();
      });
    } else {
      $(key).click(function (event) {
        event.stopPropagation();
        builder.selectKey(this);
        if (builder.lastFocus) {
          $(builder.lastFocus).focus().select();
        }
      }).dblclick(function (event) {
        event.stopPropagation();
        var layer = $(this).data('nextlayer');
        if (layer) builder.selectLayerByName(layer);
      });
      $(kid).click(function (event) {
        event.stopPropagation();
        builder.selectKey(key);
        $('#inpKeyName').focus().select();
      });
    }


    if (position == 'before') {
      builder.selectedKey().before(key);
    } else if (position == 'after') {
      builder.selectedKey().after(key);
    } else if (position == 'before-subkey') {
      builder.selectedSubKey().before(key);
    } else if (position == 'after-subkey') {
      builder.selectedSubKey().after(key);
    } else {
      $(position).append(key);
    }

    $(key).draggable({
      revert: "invalid",
      stack: ".key",
      zIndex: 100,
      helper: "clone",
      //appendTo: "body",
      start: function (event, ui) {
        var isSubKey = ($(this).parent().attr('id') == 'sk');
        if (isSubKey)
          builder.selectSubKey(null);
        else
          builder.selectKey(null, false);
        var parent = isSubKey ? '#sk' : '#kbd';
        var drag = this;
        drag.overList = [];
        var pos = $(this).position();
        $(this).addClass('key-dragging');
        $(parent + ' .key').before(function (index) {
          if (this == drag) return '';
          if ($(this).prev()[0] == drag) return '<div class="key-droppable key-current"></div>';
          return '<div class="key-droppable"></div>';
        });
        $(isSubKey ? '#sk' : '.row').append('<div class="key-droppable"></div>');

        $('.key-droppable').css('margin-top', $(this).css('margin-top')).css('height', $(this).css('height')).droppable({
          accept: ".key",
          tolerance: "touch",
          over: function (event, ui) {
            drag.overList.push(this);
            $(drag.overList[0]).addClass('key-droppable-hover');
          },
          out: function (event, ui) {
            var n = drag.overList.indexOf(this);
            if (n >= 0)
              drag.overList.splice(n, 1);
            $(this).removeClass('key-droppable-hover');
            if (drag.overList.length > 0) $(drag.overList[0]).addClass('key-droppable-hover');
          },
          drop: function (event, ui) {
            //
            // Drop the selected key into its new position
            //
            builder.saveUndo();
            $(drag).detach().removeClass('key-dragging');
            $(drag.overList[0]).after(drag);
            //builder.selectKey(drag);
            if (!isSubKey) builder.rescale(); else builder.generateSubKeys();
          }
        });
        $('.key-current').css('width', $(drag).width() + 'px').css('margin-left', -($(drag).width()) + 'px');
      },
      stop: function (event, ui) {
        $('.key-droppable').remove();
        $(this).removeClass('key-dragging');
        //builder.rescale();
      }
    });

    builder.formatKey(key, sp);

    return key;
  };

  this.formatKey = function (key, sp) {
    $(key)
      .removeClass('key-modifier')
      .removeClass('key-modifier-selected')
      .removeClass('key-deadkey')
      .removeClass('key-blank')
      .removeClass('key-spacer');
    if (sp) {
      switch (parseInt(sp, 10)) {
        case 1: $(key).addClass('key-modifier'); break;
        case 2: $(key).addClass('key-modifier-selected'); break;
        case 8: $(key).addClass('key-deadkey'); break;
        case 9: $(key).addClass('key-blank'); break;
        case 10: $(key).addClass('key-spacer'); break;
      }
    }
  }

  this.delSelectedKey = function () {
    var key = builder.selectedKey();
    var nextKey = $(key).next('.key');
    if (nextKey.length == 0) nextKey = $(key).prev('.key');
    if (nextKey.length == 0) {
      return this.delSelectedRow();
    }
    $(key).remove();
    builder.selectKey(nextKey);
  }

  this.delSelectedRow = function () {
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
  }

  this.selectSubKey = function (key) {
    builder.selectedSubKey().removeClass('selected');
    if (key) {
      $(key).addClass('selected');
      $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,input#inpSubKeyCap').css('display', 'block');
      $('div#btnDelSubKey').css('display', ($('#sk > div').length > 1) ? 'block' : '');
      var offset = $(key).offset();

      $('#wedgeAddSubKeyLeft').offset({ left: offset.left - 9, top: offset.top + $(key).outerHeight() + 2 });
      $('#wedgeAddSubKeyRight').offset({ left: offset.left + $(key).outerWidth() - 7, top: offset.top + $(key).outerHeight() + 2 });
      $('div#btnDelSubKey').offset({ left: offset.left + $(key).outerWidth() - 14, top: offset.top + 3 });
      $('input#inpSubKeyCap').offset({ left: offset.left + 16, top: offset.top + 4 }).width($(key).width() - 32);
      builder.lastFocus = $('input#inpSubKeyCap');
      this.prepareSubKey();
      builder.enableSubKeyControls();
    } else {
      $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,div#btnDelSubKey,input#inpSubKeyCap').css('display', '');
      builder.enableSubKeyControls();
    }
  }

  this.selectKey = function (key, adjustResizable) {

    if (arguments.length == 1 || adjustResizable)
      builder.selectedKey().resizable('destroy');

    $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,div#btnDelSubKey,input#inpSubKeyCap').css('display', '');
    builder.selectedKey().removeClass('selected');
    if (key) {
      $(key).addClass('selected');
      $('.kcontrol.wedge-horz,.kcontrol.wedge-vert,div#btnDelKey,input#inpKeyCap').css('display', 'block');
      var rowOffset = $(key).parent().offset();
      var offset = $(key).offset();

      $('#wedgeAddRowAbove').offset({ left: rowOffset.left - 18, top: rowOffset.top - 7 });
      $('#wedgeAddRowBelow').offset({ left: rowOffset.left - 18, top: rowOffset.top + $(key).parent().outerHeight() - 7 });
      $('#wedgeAddKeyLeft').offset({ left: offset.left - 9, top: offset.top + $(key).outerHeight() + 2 });
      $('#wedgeAddKeyRight').offset({ left: offset.left + $(key).outerWidth() - 7, top: offset.top + $(key).outerHeight() + 2 });
      $('div#btnDelKey').offset({ left: offset.left + $(key).outerWidth() - 14, top: offset.top + 3 });
      $('input#inpKeyCap').offset({ left: offset.left + 16, top: offset.top + 4 }).width($(key).width() - 32);
      this.prepareKey();
      builder.lastFocus = $('input#inpKeyCap');
    } else {
      $('.kcontrol.wedge-horz,.kcontrol.wedge-vert,div#btnDelKey,input#inpKeyCap').css('display', '');
      $('#sk div').remove();
      builder.enableKeyControls();
      builder.enableSubKeyControls();
    }
  };

  this.selectedKey = function () {
    return $('#kbd .selected');
  }

  $('#inpKeyPadding').change(function () {
    builder.saveUndo();
    builder.selectedKey().data('pad', $(this).val())
                         .css('margin-left', $(this).val() + 'px');
    builder.rescale();
  });

  $('#inpKeyWidth').change(function () {
    builder.saveUndo();
    builder.selectedKey().data('width', $(this).val())
                         .css('width', parseInt($(this).val(), 10) * this.xscale + 'px');
    builder.rescale();
  });

  $('#inpKeyName').change(function () {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
    builder.selectedKey().data('id', $(this).val());
    builder.updateKeyId(builder.selectedKey());
  }).autocomplete({
    source: builder.lookupKeyNames,
    change: function () {
      if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
      builder.selectedKey().data('id', $(this).val());
      builder.updateKeyId(builder.selectedKey());
    }
  }).keyup(function () {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
    builder.selectedKey().data('id', $(this).val());
    builder.updateKeyId(builder.selectedKey());
  }).blur(function () {
    builder.hasSavedKeyUndo = false;
  });

  this.updateSelectedKeyText = function (val) {
    var k = builder.selectedKey();
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);

    builder.updateCharacterMap(val, false);
  }

  this.updateCharacterMap = function (val, fromSubKey) {
    // Update character map
    var src = $(fromSubKey ? '#inpSubKeyCap' : '#inpKeyCap')[0];

    var x1 = src.selectionStart;
    var x2 = src.selectionEnd;
    if (x2 != x1 || x1 > 0) {
      var ch = val.charCodeAt(x2 - 1);
      if (x2 > 1 && ch >= 0xDC00 && ch < 0xE000) {
        // yep, surrogate pairs again!
        var chHigh = val.charCodeAt(x2 - 2), chLow = ch;
        ch = ((chHigh - 0xD800) << 10) + (chLow - 0xDC00) + 0x10000;
      }
      builder.command('selected-char?code=' + ch.toString());
    }
  }

  $('#inpKeyCap').change(function () {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
    builder.updateSelectedKeyText($(this).val());
  }).autocomplete({
    source: builder.specialKeyNames,
    change: function () {
      if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
      builder.updateSelectedKeyText($(this).val());
    }
  }).keyup(function () {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
    builder.updateSelectedKeyText($(this).val());
  }).mouseup(function () {
    builder.updateCharacterMap($(this).val(), false);
  }).focus(function () {
    builder.updateCharacterMap($(this).val(), false);
  }).blur(function () {
    builder.hasSavedKeyUndo = false;
  });


  $('#selKeyType').change(function () {
    builder.saveUndo();
    var sp = $(this).val();
    if (sp == 0) {
      builder.selectedKey().removeData('sp');
    } else {
      builder.selectedKey().data('sp', $(this).val());
    }
    builder.formatKey(builder.selectedKey(), $(this).val());
  });

  $('#selKeyNextLayer').change(function () {
    builder.saveUndo();
    $(this).val() === '' ?
      builder.selectedKey().removeData('nextlayer') :
      builder.selectedKey().data('nextlayer', $(this).val());
  });

  $('#selKeyLayerOverride').change(function () {
    builder.saveUndo();
    $(this).val() === '' ?
      builder.selectedKey().removeData('layer') :
      builder.selectedKey().data('layer', $(this).val());
    builder.updateKeyId(builder.selectedKey());
  });

  this.prepareKey = function () {
    builder.enableKeyControls();
    var key = builder.selectedKey();
    builder.hasSavedKeyUndo = false;
    $('#inpKeyCap').val($(key).data('text'));
    $('#inpKeyName').val($(key).data('id'));
    $('#inpKeyWidth').val($(key).data('width'));
    $('#inpKeyPadding').val($(key).data('pad'));
    $('#selKeyType').val($(key).data('sp') ? $(key).data('sp') : 0);
    $('#selKeyNextLayer').val($(key).data('nextlayer'));
    $('#selKeyLayerOverride').val($(key).data('layer'));

    $(key).resizable({
      minWidth: 10 * this.xscale,
      minHeight: 100 * this.yscale,
      maxWidth: 500 * this.xscale,
      maxHeight: 100 * this.yscale,
      ghost: false,
      handles: "e",
      resize: function (event, ui) {
        builder.selectedKey().data('width', Math.round(ui.size.width / builder.xscale, 0));
        $('#inpKeyWidth').val(Math.round(ui.size.width / builder.xscale, 0));
      },
      start: function (event, ui) {
        builder.saveUndo();
        $(ui.originalElement).css('position', 'relative');
        $(ui.originalElement).css('left', '');
      },
      stop: function (event, ui) {
        builder.rescale();
      }
    });

    //
    // Prepare sub-key array
    //

    $('#sk div').remove();
    var skContainer = $('#sk');
    var sk = $(key).data('sk');
    if (typeof sk != 'object') {
      $(key).removeClass('hasSubKeyArray');
      builder.enableSubKeyControls();
      return;
    }

    $(key).addClass('hasSubKeyArray');
    for (var i = 0; i < sk.length; i++) {
      var nkey = builder.addKey(skContainer, true);
      var key = sk[i];

      var code = key.id ? String.fromCharCode(parseInt(key.id.substring(2), 16)) : 0;
      var text = typeof key.text == 'string' ? key.text : (code.charCodeAt(0) < 32 ? '' : code);

      $(nkey)
        .data('text', text)
        .data('id', key.id)
        .data('sp', key.sp)
        .data('font', key.font)
        .data('fontsize', key.fontsize)
        .data('nextlayer', key.nextlayer)
        .data('layer', key.layer)
        .css('width', (100 * 0.7) + 'px')
        .css('font-family', KVKL[builder.lastPlatform].font);

      $('.text', nkey).text(this.renameSpecialKey(text));
      builder.updateKeyId(nkey);
    }
    //var div = document.createElement('div'); $(div).css('clear', 'both'); $('#sk').append(div);
    builder.selectSubKey($('#sk > div')[0]);
    builder.prepareSubKey();
    builder.enableSubKeyControls();
  }

  this.enableKeyControls = function () {
    var key = builder.selectedKey();
    if (key.length == 0) {
      $('#keyToolbar *').attr('disabled', 'disabled');
      $('#btnAddSubKeyArray').css('display', '');
      $('#btnDelSubKeyArray').css('display', 'none');
    } else {
      $('#keyToolbar *').removeAttr('disabled');
      if (builder.selectedKey().hasClass('hasSubKeyArray')) {
        $('#btnAddSubKeyArray').css('display', 'none');
        $('#btnDelSubKeyArray').css('display', '');
      } else {
        $('#btnAddSubKeyArray').css('display', '');
        $('#btnDelSubKeyArray').css('display', 'none');
      }
    }
  }
  this.enableSubKeyControls = function () {
    var key = builder.selectedSubKey();
    if (key.length == 0) {
      $('#subKeyToolbar').css('visibility', 'hidden'); // attr('disabled', 'disabled');
    } else {
      $('#subKeyToolbar').css('visibility', ''); // removeAttr('disabled');
    }
  }

  this.rescale = function () {
    builder.saveUndo();
    var keyId = builder.selectedKey().data('id'); //$('#kbd .key')
    builder.prepareLayer();
    if (keyId !== null)
      builder.selectKey($('#kbd .key').filter(function (index) { return $(this).data('id') === keyId; }));
  }

  //
  // Popup keys
  //

  this.addSubKeyArray = function () {
    var key = builder.selectedKey();
    if ($(key).data('sk')) return;
    var sk = [{ id: 'T_new_' + builder.uniqId}];
    builder.uniqId++;
    $(key).data('sk', sk);
    builder.prepareKey();
  }

  this.delSubKeyArray = function () {
    var key = builder.selectedKey();
    $(key).removeData('sk');
    builder.prepareKey();
    builder.selectSubKey(null);
    builder.enableSubKeyControls();
  }

  this.delSelectedSubKey = function () {
    var key = builder.selectedSubKey();
    if (key.length == 0) return;
    var nextKey = $(key).next('.key');
    if (nextKey.length == 0) nextKey = $(key).prev('.key');
    if (nextKey.length == 0) return;
    $(key).remove();
    builder.selectSubKey(nextKey);
  };

  this.selectedSubKey = function () {
    return $('#sk .selected');
  };

  this.generateSubKeys = function () {
    var key = builder.selectedKey();
    var sk = [];
    var keys = $('#sk > div.key').filter(':not(.ui-draggable-dragging)');
    for (var i = 0; i < keys.length; i++) {
      sk.push({
        text: $(keys[i]).data('text'),
        id: $(keys[i]).data('id'),
        sp: $(keys[i]).data('sp'),
        font: $(keys[i]).data('font'),
        fontsize: $(keys[i]).data('fontsize'),
        nextlayer: $(keys[i]).data('nextlayer'),
        layer: $(keys[i]).data('layer')
      });
    }
    key.data('sk', sk);
  }

  this.prepareSubKey = function () {
    var key = builder.selectedSubKey();

    $('#inpSubKeyCap').val($(key).data('text'));
    $('#inpSubKeyName').val($(key).data('id'));
    $('#selSubKeyType').val($(key).data('sp') ? $(key).data('sp') : 0);
    $('#selSubKeyNextLayer').val($(key).data('nextlayer'));
    $('#selSubKeyLayerOverride').val($(key).data('layer'));
  }

  var subKeyNameChange = function () {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedKeyUndo = true; }
    builder.saveUndo();
    var nkey = builder.selectedSubKey();
    if (nkey.length == 0) return;
    nkey.data('id', $(this).val());
    builder.generateSubKeys();
    builder.updateKeyId(nkey);
  };

  $('#inpSubKeyName')
    .change(subKeyNameChange)
    .keyup(subKeyNameChange)
    .autocomplete({
      source: builder.lookupKeyNames,
      change: subKeyNameChange
    }).blur(function () {
      builder.hasSavedKeyUndo = false;
    });

  $('#inpSubKeyCap').change(function () {
    builder.saveUndo();

    var val = $(this).val();

    var k = builder.selectedSubKey();
    if (k.length == 0) return;
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);

    builder.updateCharacterMap(val, true);
    builder.generateSubKeys();
  });

  builder.updateSubKeyCap = function (val) {
    if (!builder.hasSavedKeyUndo) { builder.saveUndo(); builder.hasSavedSubKeyUndo = true; }
    var k = builder.selectedSubKey();
    if (k.length == 0) return;
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);

    builder.updateCharacterMap(val, true);
    builder.generateSubKeys();
  }

  $('#inpSubKeyCap').change(function () {
    builder.updateSubKeyCap($(this).val());
  }).autocomplete({
    source: builder.specialKeyNames,
    change: function () {
      builder.updateSubKeyCap($(this).val());
    }
  }).keyup(function () {
    builder.updateSubKeyCap($(this).val());
  }).mouseup(function () {
    builder.updateCharacterMap($(this).val(), false);
  }).focus(function () {
    builder.updateCharacterMap($(this).val(), false);
  }).blur(function () {
    builder.hasSavedSubKeyUndo = false;
  });

  $('#selSubKeyNextLayer').change(function () {
    builder.saveUndo();
    $(this).val() === '' ?
      builder.selectedSubKey().removeData('nextlayer') :
      builder.selectedSubKey().data('nextlayer', $(this).val());
    builder.generateSubKeys();
  });

  $('#selSubKeyLayerOverride').change(function () {
    builder.saveUndo();
    $(this).val() === '' ?
      builder.selectedSubKey().removeData('layer') :
      builder.selectedSubKey().data('layer', $(this).val());
    builder.updateKeyId(builder.selectedSubKey());
    builder.generateSubKeys();
  });

  $('#wedgeAddSubKeyLeft').click(function () {
    builder.saveUndo();
    builder.selectSubKey(builder.addKey('before-subkey', true));
    builder.generateSubKeys();
  });

  $('#wedgeAddSubKeyRight').click(function () {
    builder.saveUndo();
    builder.selectSubKey(builder.addKey('after-subkey', true));
    builder.generateSubKeys();
  });

  $('#btnDelSubKey').click(function () {
    builder.saveUndo();
    builder.delSelectedSubKey();
    builder.generateSubKeys();
  });

  $('#selSubKeyType').change(function () {
    builder.saveUndo();
    var sp = $(this).val(), key = builder.selectedSubKey();
    if (key.length == 0) return;
    if (sp == 0) {
      key.removeData('sp');
    } else {
      key.data('sp', $(this).val());
    }
    builder.formatKey(key, $(this).val());
    builder.generateSubKeys();
  });

  this.generate = function (display) {
    var layer = KVKL[builder.lastPlatform].layer[builder.lastLayerIndex];
    layer.row = [];

    var rows = $('#kbd > div.row'), n = 1;
    $(rows).each(function () {
      var row = { id: n++, key: [] };
      $(this).children(".key").each(function () {
        if ($(this).hasClass('ui-draggable-dragging')) return;
        var key = { "id": $(this).data('id'), "text": $(this).data('text') };
        key.pad = $(this).data('pad');
        key.width = $(this).data('width');
        key.sp = $(this).data('sp');
        key.font = $(this).data('font');
        key.fontsize = $(this).data('fontsize');
        key.nextlayer = $(this).data('nextlayer');
        key.layer = $(this).data('layer');
        key.sk = $(this).data('sk');
        row.key.push(key);
      });
      layer.row.push(row);
    });

    //KVKL[builder.lastPlatform].layer[builder.lastLayerIndex] = layer;

    if (!display) {
      $('#data').val(JSON.stringify(KVKL, null, '  '));
    }
  }


  $('#wedgeAddRowAbove').click(function () {
    builder.saveUndo();
    var row = builder.addRow('above'); builder.selectKey(builder.addKey(row, false));
    builder.rescale();
  });

  $('#wedgeAddRowBelow').click(function () {
    builder.saveUndo();
    var row = builder.addRow('below'); builder.selectKey(builder.addKey(row, false));
    builder.rescale();
  });

  $('#wedgeAddKeyLeft').click(function () {
    builder.saveUndo();
    builder.selectKey(builder.addKey('before', false));
    builder.rescale();
  });

  $('#wedgeAddKeyRight').click(function () {
    builder.saveUndo();
    builder.selectKey(builder.addKey('after', false));
    builder.rescale();
  });

  $('#btnDelKey').click(function () {
    builder.saveUndo();
    builder.delSelectedKey();
    builder.rescale();
  });

  $('#btnAddSubKeyArray').click(function () {
    builder.saveUndo();
    builder.addSubKeyArray();
    builder.selectSubKey($('#sk > div')[0]);
    if (builder.lastFocus) builder.lastFocus.focus();
    builder.enableKeyControls();
  });

  $('#btnDelSubKeyArray').click(function () {
    builder.saveUndo();
    builder.delSubKeyArray();
    builder.enableKeyControls();
  });

  $('#btnGenerate').click(function () { builder.generate(); });

  $('#selPlatform').change(function () {
    if (builder.lastPlatform) builder.generate();
    builder.selectPlatform();
  });

  $('#selLayer').change(function () {
    if (builder.lastPlatform) builder.generate();
    builder.selectLayer();
  });

  //
  // Platform dialogs
  //

  $('#addPlatformDialog').dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {
        builder.saveUndo();
        builder.generate();

        var platform = $('#selAddPlatform').val();
        KVKL[platform] = $.extend(true, {}, KVKL[builder.lastPlatform]);  // copy existing platform

        builder.preparePlatforms();
        $('#selPlatform').val(platform);
        builder.selectPlatform();

        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }
  });

  $('#btnAddPlatform').click(function () {
    $('#selAddPlatform option').remove();
    var platforms = { 'tablet': 1, 'desktop': 1, 'phone': 1 };
    for (var platform in KVKL) {
      platforms[platform] = 0;
    }
    for (platform in platforms) {
      if (platforms[platform]) {
        var opt = document.createElement('option');
        $(opt).text(platform);
        $('#selAddPlatform').append(opt);
      }
    }
    $('#addPlatformDialog').dialog('open')
  });

  $('#btnDelPlatform').click(function () {
    if ($('#selPlatform option').length == 1) return;
    builder.saveUndo();
    var platform = $('#selPlatform').val();
    delete KVKL[platform];
    builder.lastPlatform = null;
    builder.preparePlatforms();
  });

  //
  // Layer dialogs
  //

  $('#layerPropertiesDialog').dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {
        var newLayerName = $('#layerName').val();
        if (!newLayerName.match(/^[a-zA-Z0-9_]+$/)) {
          alert('Layer name must contain only alphanumerics and underscore.');
          return;
        }

        builder.saveUndo();
        builder.generate();

        var platform = KVKL[builder.lastPlatform];
        var oldLayerName = platform.layer[builder.lastLayerIndex].id;

        for (var i = 0; i < platform.layer.length; i++) {
          var layer = platform.layer[i];
          if (layer.row) {
            for (var j = 0; j < layer.row.length; j++) {
              var row = layer.row[j];
              if (row.key) {
                for (var k = 0; k < row.key.length; k++) {
                  var key = row.key[k];
                  if (key.layer == oldLayerName) {
                    key.layer = newLayerName;
                  }
                  if (key.nextlayer == oldLayerName) {
                    key.nextlayer = newLayerName;
                  }

                  if (key.sk) {
                    for (var l = 0; l < key.sk.length; l++) {
                      if (key.sk[l].layer == oldLayerName) {
                        key.sk[l].layer = newLayerName;
                      }
                      if (key.sk[l].nextlayer == oldLayerName) {
                        key.sk[l].nextlayer = newLayerName;
                      }
                    }
                  }
                }
              }
            }
          }
        }

        platform.layer[builder.lastLayerIndex].id = newLayerName;
        builder.prepareLayers();
        $('#selLayer').val(builder.lastLayerIndex);
        builder.selectLayer();
        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }
  });

  $('#selectKeyDialog').dialog({
    autoOpen: false,
    height: 140,
    width: 240,
    modal: true,
    buttons: {
      "Cancel": function () {
        builder.nextKeySelects = false;
        $(this).dialog('close');
      }
    }
  });

  $('#addLayerDialog').dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {
        if (!$('#addLayerName').val().match(/^[a-zA-Z0-9_]+$/)) {
          alert('Layer name must contain only alphanumerics and underscore.');
          return;
        }
        builder.saveUndo();
        builder.generate();
        var layer = $.extend(true, {}, KVKL[builder.lastPlatform].layer[builder.lastLayerIndex]);
        layer.id = $('#addLayerName').val();
        var n = KVKL[builder.lastPlatform].layer.push(layer) - 1;
        builder.selectPlatform();
        $('#selLayer').val(n);
        builder.selectLayer();
        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }
  });

  $('#btnAddLayer').click(function () {
    $('#addLayerDialog').dialog('open');
  });

  $('#btnDelLayer').click(function () {
    if ($('#selLayer option').length == 1) return;
    builder.saveUndo();
    KVKL[builder.lastPlatform].layer.splice(builder.lastLayerIndex, 1);
    builder.selectPlatform();
  });

  $('#btnEditLayer').click(function () {
    $('#layerName').val(KVKL[builder.lastPlatform].layer[builder.lastLayerIndex].id);
    $('#layerPropertiesDialog').dialog('open');
  });

  $('input').blur(function () {
    builder.lastFocus = this;
  });

  $('input').focus(function () {
    builder.lastFocus = this;
  });

  $('#btnUndo').click(function () {
    builder.undo();
  });

  $('#btnRedo').click(function () {
    builder.redo();
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

  $('#sk').click(function () {
    builder.selectSubKey(null);
  });

  builder.ctrlDown = false;
  builder.nextKeySelects = false;

  builder.selectKeyByCode = function (code) {
    if (code >= 0 && code < 256) {
      var keyName = builder.standardKeyNames[code];
      var key = $('.key').filter(function (index) { return $(this).data('id') === keyName; });
      if (key.length > 0) builder.selectKey(key[0]);
    }
  }

  $(document).keydown(function (event) {
    builder.ctrlDown = (event.which == 17);
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

  builder.preparePlatforms();
  builder.enableUndoControls();
});
  