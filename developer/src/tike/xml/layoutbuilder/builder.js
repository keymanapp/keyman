$(function() {
  (function(search) {
    var q = search.match(/Filename=(.+)(&|$)/);
    if(q) {
      builder.filename = decodeURIComponent(q[1]);
    }
  })(location.search);

  this.xscale = 1;
  this.yscale = 1;
  this.uniqId = 1;


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
    builder.saveState();
  });

  $('#chkShowAllModifierOptions').click(function () {
    builder.showAllModifierCombinations = $('#chkShowAllModifierOptions')[0].checked;
    builder.fillModifierSelect();
    builder.prepareKey();
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
    //Note:  U+E000 *is* PUA but was not accepted by IE as a character in the EOT font, so Alt recoded as U+E019
    return this.specialCharacters[oldText] ?
      String.fromCharCode(0xE000 + this.specialCharacters[oldText]) :
      oldText;
  }

  this.prepareLayers = function () {
    this.fillLayerSelect();
    this.fillModifierSelect();
  };

  this.fillLayerSelect = function() {
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
  };

  this.fillModifierSelect = function() {
    var modifiers = this.showAllModifierCombinations ? this.validModifierCombinations : this.minimalModifierCombinations;

    var
      $selKeyLayerOverride = $('#selKeyLayerOverride'),
      $selSubKeyLayerOverride = $('#selSubKeyLayerOverride'),
      $addLayerList = $('#addLayerList');

    var add = function(e, val, text) {
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
      var name = this.getModifierCombinationName(modifiers[modifier]);

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

    if(!this.showAllModifierCombinations) {
      // Add any layer names that are already referenced
      for(modifier = 0; modifier < this.validModifierCombinations.length; modifier++) {
        var name = this.getModifierCombinationName(this.validModifierCombinations[modifier]);
        if(!alreadyAdded(this.validModifierCombinations[modifier]) && isUsed(name)) {
          add($selKeyLayerOverride, name);
          add($selSubKeyLayerOverride, name);
        }
      }
    }
  };

  this.getModifierCombinationFromLayerId = function(id) {
    for(var i = 0; i < this.validModifierCombinations.length; i++) {
      if(this.getModifierCombinationName(this.validModifierCombinations[i]) == id) {
        return this.validModifierCombinations[i];
      }
    }
    return 0;
  };

  this.isLayerIdShifted = function(id) {
    return (builder.getModifierCombinationFromLayerId(id) & this.modifierCodes.SHIFT) != 0;
  };

  this.prepareLayer = function () {
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

        if(this.specialCharacters[text])
          $(nkey).addClass('key-special-text');

        $('.text', nkey).text(this.renameSpecialKey(text));
        if(KVKL[builder.lastPlatform].displayUnderlying) $('.underlying', nkey).text(this.getStandardKeyCap(key.id, key.layer ? builder.isLayerIdShifted(key.layer) : isLayerShifted));

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

  this.getStandardKeyCap = function (id, shifted) {
    id = id ? id.toUpperCase() : '';
    var i = this.standardKeyNames.findIndex(function(x) { return x.toUpperCase() == id });
    return i >= 0 ? this.standardKeyCaps[i][shifted ? 1 : 0] : '';
  };

  this.updateKeyId = function (nkey) {
    $('.id', nkey).text(($(nkey).data('layer') || '') + ' ' + $(nkey).data('id'));
  };

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

    $('#chkDisplayUnderlying')[0].checked = KVKL[builder.lastPlatform].displayUnderlying;

    builder.prepareLayers();
    builder.selectLayer(0);
  }

  this.selectLayer = function (val) {
    if(val) $('#selLayer').val(val);
    builder.lastLayerIndex = $('#selLayer').val();
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


  this.commands = [];

  this.command = function (cmd) {
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
    var kunderlying = document.createElement('div');
    $(kid).addClass('id');
    $(ktext).addClass('text');
    $(kunderlying).addClass('underlying');
    $(key).append(kid);
    $(key).append(ktext);
    $(key).append(kunderlying);
    $(key).addClass('key');
    $(key).data('id', 'T_new_' + this.uniqId);
    builder.uniqId++;
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
            builder.post();
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


  this.selectKey = function (key, adjustResizable) {

    if (arguments.length == 1 || adjustResizable)
      builder.selectedKey().resizable('destroy');

    $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,div#btnDelSubKey,input#inpSubKeyCap').css('display', '');
    builder.selectedKey().removeClass('selected');
    if (key && $(key).length) {
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
    builder.saveState();
  };

  this.selectedKey = function () {
    return $('#kbd .selected');
  }

  this.wrapChange = function(f, opt) {
    return function() {
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
      var r = f.apply(this);
      if(typeof opt == 'object' && opt.rescale) {
        builder.rescale();
      }
      builder.post();
      return r;
    }
  };

  const inpKeyPaddingChange = builder.wrapChange(function () {
    builder.selectedKey().data('pad', $(this).val())
                         .css('margin-left', $(this).val() + 'px');
  }, {rescale: true});

  $('#inpKeyPadding')
    .change(inpKeyPaddingChange)
    .on('input', inpKeyPaddingChange);

  const inpKeyWidthChange = builder.wrapChange(function () {
    builder.selectedKey().data('width', $(this).val())
                         .css('width', parseInt($(this).val(), 10) * this.xscale + 'px');
  }, {rescale: true});

  $('#inpKeyWidth')
    .change(inpKeyWidthChange)
    .on('input', inpKeyWidthChange);

  const inpKeyNameChange = builder.wrapChange(function () {
    builder.selectedKey().data('id', $(this).val());
    builder.updateKeyId(builder.selectedKey());
  }, {saveOnce: true});

  const wrapInstant = function(f) {
    return function() {
      window.setTimeout(f.bind(this), 0);
    }
  }

  $('#inpKeyName')
    .change(inpKeyNameChange)
    .autocomplete({
      source: builder.lookupKeyNames,
      change: inpKeyNameChange,
      select: wrapInstant(inpKeyNameChange)
    })
    .on('input', inpKeyNameChange)
    .blur(function () {
      builder.hasSavedKeyUndo = false;
    });

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
      builder.command('selected-char,' + ch.toString());
    }
  }

  const inpKeyCapChange = builder.wrapChange(function (e) {
    const val = $(this).val();
    var k = builder.selectedKey();
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);

    if(builder.specialCharacters[val]) {
      k.addClass('key-special-text');
    } else {
      k.removeClass('key-special-text');
    }

    builder.updateCharacterMap(val, false);
  }, {saveOnce: true});

  $('#inpKeyCap')
    .on('input', inpKeyCapChange)
    .change(inpKeyCapChange)
    .autocomplete({
      source: builder.specialKeyNames,
      change: inpKeyCapChange,
      select: wrapInstant(inpKeyCapChange)
    })
    .mouseup(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).focus(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).blur(function () {
      builder.hasSavedKeyUndo = false;
    });

  const selKeyTypeChange = builder.wrapChange(function () {
    var sp = $(this).val();
    if (sp == 0) {
      builder.selectedKey().removeData('sp');
    } else {
      builder.selectedKey().data('sp', $(this).val());
    }
    builder.formatKey(builder.selectedKey(), $(this).val());
  });

  $('#selKeyType').change(selKeyTypeChange);

  const selKeyNextLayerChange = builder.wrapChange(function () {
    $(this).val() === '' ?
      builder.selectedKey().removeData('nextlayer') :
      builder.selectedKey().data('nextlayer', $(this).val());
  });

  $('#selKeyNextLayer').change(selKeyNextLayerChange);

  const selKeyLayerOverrideChange = builder.wrapChange(function () {
    $(this).val() === '' ?
      builder.selectedKey().removeData('layer') :
      builder.selectedKey().data('layer', $(this).val());
    builder.updateKeyId(builder.selectedKey());
  });

  $('#selKeyLayerOverride').change(selKeyLayerOverrideChange);

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

  this.rescale = function () {
    builder.saveUndo();
    var keyId = builder.selectedKey().data('id');
    builder.prepareLayer();
    if (keyId !== null)
      builder.selectKey($('#kbd .key').filter(function (index) { return $(this).data('id') === keyId; }).first());
  };

  this.generate = function (display, force) {
    var json = JSON.stringify(KVKL, null, '  ');

    if(!display) {
      // Save changed settings -- only when not saving undo
      KVKL[builder.lastPlatform].displayUnderlying = $('#chkDisplayUnderlying')[0].checked;
    }

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

    builder.saveJSON(json, force);
  };

  this.saveJSON = function(force) {
    builder.removeEmptyRows();
    var newJson = JSON.stringify(KVKL, null, '  ');
    if(force || newJson != json) {
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
    }
  };

  this.post = this.generate;

  $('#wedgeAddRowAbove').click(builder.wrapChange(function () {
    var row = builder.addRow('above'); builder.selectKey(builder.addKey(row, false));
  }, {rescale: true}));

  $('#wedgeAddRowBelow').click(builder.wrapChange(function () {
    var row = builder.addRow('below'); builder.selectKey(builder.addKey(row, false));
  }, {rescale: true}));

  $('#wedgeAddKeyLeft').click(builder.wrapChange(function () {
    builder.selectKey(builder.addKey('before', false));
  }, {rescale: true}));

  $('#wedgeAddKeyRight').click(builder.wrapChange(function () {
    builder.selectKey(builder.addKey('after', false));
  }, {rescale: true}));

  $('#btnDelKey').click(builder.wrapChange(function () {
    builder.delSelectedKey();
  }, {rescale: true}));

  $('#btnGenerate').click(function () { builder.generate(false,false); });

  $('#chkDisplayUnderlying').click(function () {
    builder.saveUndo();
    var platform = $('#selPlatform').val();
    builder.post();
    builder.prepareLayer();
  });


  $('input').blur(function () {
    builder.lastFocus = this;
  });

  $('input').focus(function () {
    builder.lastFocus = this;
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
    if(!event.originalEvent.repeat) {
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

  builder.loadingState = true;

  builder.saveState = function() {
    if(builder.loadingState) return;

    var state = {
      platform: builder.lastPlatform,
      layer: builder.lastLayerIndex,
      presentation: $('#selPlatformPresentation').val()
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
  };

  builder.loadState = function() {
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
          if(data.presentation && builder.presentations[data.presentation]) {
            $('#selPlatformPresentation').val(data.presentation);
            builder.prepareLayer();
          }
          if(data.layer && KVKL[builder.lastPlatform][data.layer]) {
            $('#selLayer').val(data.layer);
            builder.selectLayer();
          }
          if(data.key) {
            builder.selectKey($('#kbd .key').filter(function (index) { return $(this).data('id') === data.key; }).first());
          }
          if(data.subkey) {
            builder.selectSubKey($('#sk .key').filter(function (index) { return $(this).data('id') === data.subkey; }).first());
          }
        }
      }
    );
  };

  builder.removeEmptyRows = function() {
    var json = JSON.stringify(KVKL, null, '  ');
    for (var platform in KVKL) {
      for (let layer of KVKL[platform].layer) {
        let newRow = [], n = 1;
        for(let row of layer.row) {
          if(row.key.length > 0) {
            row.id = n;
            n++;
            newRow.push(row);
          }
        }
        layer.row = newRow;
      }
    }
  };

}.bind(builder));

function initBuilder() {
  $(function() {
    builder.preparePlatforms();
    builder.enableUndoControls();
    builder.loadState();
  });
}