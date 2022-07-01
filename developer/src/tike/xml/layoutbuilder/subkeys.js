$(function() {

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

  var subKeyNameChange = builder.wrapChange(function () {
    var nkey = builder.selectedSubKey();
    if (nkey.length == 0) return;
    nkey.data('id', $(this).val());
    builder.generateSubKeys();
    builder.updateKeyId(nkey);
  }, {saveOnce: true});

  $('#inpSubKeyName')
    .change(subKeyNameChange)
    .on('input', subKeyNameChange)
    .autocomplete({
      source: builder.lookupKeyNames,
      change: subKeyNameChange,
      select: wrapInstant(subKeyNameChange)
    }).blur(function () {
      builder.hasSavedKeyUndo = false;
    });

  builder.updateSubKeyCap = function (val) {
    var k = builder.selectedSubKey();
    if (k.length == 0) return;
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);

    builder.updateCharacterMap(val, true);
    builder.generateSubKeys();
  };

  const inpSubKeyCapChange = builder.wrapChange(function () {
    builder.updateSubKeyCap($(this).val());
  }, {saveOnce: true});

  $('#inpSubKeyCap')
    .change(inpSubKeyCapChange)
    .autocomplete({
      source: builder.specialKeyNames,
      change: inpSubKeyCapChange,
      select: wrapInstant(inpSubKeyCapChange)
    })
    .on('input', inpSubKeyCapChange)
    .mouseup(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).focus(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).blur(function () {
      builder.hasSavedSubKeyUndo = false;
    });

  const selSubKeyNextLayerChange = builder.wrapChange(function () {
    $(this).val() === '' ?
      builder.selectedSubKey().removeData('nextlayer') :
      builder.selectedSubKey().data('nextlayer', $(this).val());
    builder.generateSubKeys();
  });

  $('#selSubKeyNextLayer').change(selSubKeyNextLayerChange);

  const selSubKeyLayerOverrideChange = builder.wrapChange(function () {
    $(this).val() === '' ?
      builder.selectedSubKey().removeData('layer') :
      builder.selectedSubKey().data('layer', $(this).val());
    builder.updateKeyId(builder.selectedSubKey());
    builder.generateSubKeys();
  });

  $('#selSubKeyLayerOverride').change(selSubKeyLayerOverrideChange);

  $('#wedgeAddSubKeyLeft').click(builder.wrapChange(function () {
    builder.selectSubKey(builder.addKey('before-subkey', true));
    builder.generateSubKeys();
  }));

  $('#wedgeAddSubKeyRight').click(builder.wrapChange(function () {
    builder.selectSubKey(builder.addKey('after-subkey', true));
    builder.generateSubKeys();
  }));

  $('#btnDelSubKey').click(builder.wrapChange(function () {
    builder.delSelectedSubKey();
    builder.generateSubKeys();
  }));

  const selSubKeyTypeChange = builder.wrapChange(function () {
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

  $('#selSubKeyType').change(selSubKeyTypeChange);

  $('#btnAddSubKeyArray').click(builder.wrapChange(function () {
    builder.addSubKeyArray();
    builder.selectSubKey($('#sk > div')[0]);
    if (builder.lastFocus) builder.lastFocus.focus();
    builder.enableKeyControls();
  }));

  $('#btnDelSubKeyArray').click(builder.wrapChange(function () {
    builder.delSubKeyArray();
    builder.enableKeyControls();
  }));

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
    builder.saveState();
  }

  this.enableSubKeyControls = function () {
    var key = builder.selectedSubKey();
    if (key.length == 0) {
      $('#subKeyToolbar').css('visibility', 'hidden');
    } else {
      $('#subKeyToolbar').css('visibility', '');
    }
  }

  $('#sk').click(function () {
    builder.selectSubKey(null);
  });

}.bind(builder));