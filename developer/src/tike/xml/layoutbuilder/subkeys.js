$(function() {

  //
  // Popup keys
  //

  this.delSubKeyArray = function (type) {
  }

  this.delSelectedSubKey = function () {
    let key = builder.selectedSubKey();
    if (key.length == 0) return;
    let nextKey = $(key).next('.key');
    if (nextKey.length == 0) nextKey = $(key).prev('.key');
    if (nextKey.length == 0) {
      $(builder.selectedKey()).removeData($(key).data('type'));
      builder.prepareKey();
      builder.selectSubKey(null);
      builder.enableSubKeyControls();
      builder.enableKeyControls();
      return;
    }
    $(key).remove();
    builder.selectSubKey(nextKey);
    builder.generateSubKeys();
    builder.addKeyAnnotations(builder.selectedKey());
  };

  this.selectedSubKey = function () {
    return $('#sub-key-groups .selected');
  };

  this.generateSubKeys = function () {

    function generateItems(parent) {
      let keys = $(parent+' > div.key').filter(':not(.ui-draggable-dragging)').get();
      let items = [];
      for (let key of keys) {
        items.push({
          text: $(key).data('text'),
          id: $(key).data('id'),
          sp: $(key).data('sp'),
          font: $(key).data('font'),
          fontsize: $(key).data('fontsize'),
          nextlayer: $(key).data('nextlayer'),
          layer: $(key).data('layer'),
          direction: $(key).data('direction')
        });
      }
      return items;
    }

    let key = builder.selectedKey();
    key.data('longpress', generateItems('#longpress'));
    key.data('flick', generateItems('#flick'));
    key.data('multitap', generateItems('#multitap'));
  }

  this.prepareSubKey = function () {
    var key = builder.selectedSubKey();

    let type = $(key).data('type'), gestureType = '';
    switch(type) {
      case 'flick': gestureType = 'Flick ('+$(key).data('direction').toUpperCase()+')'; break;
      case 'longpress': gestureType = 'Longpress'; break;
      case 'multitap': gestureType = 'Multitap'; break;
    }

    $('#inpSubKeyGestureType').val(gestureType);
    let val = $(key).data('text');
    $('#inpSubKeyCap').val(val);
    $('#inpSubKeyCapUnicode').val(builder.toUnicodeString(val));
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
      select: builder.wrapInstant(subKeyNameChange)
    }).blur(function () {
      builder.hasSavedKeyUndo = false;
    });


  const inpSubKeyCapChange = builder.wrapChange(function () {
    var k = builder.selectedSubKey();
    if (k.length == 0) return;
    let val = $(this).val();
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);
    $('#inpSubKeyCapUnicode').val(builder.toUnicodeString(val));

    builder.updateCharacterMap(val, true);
    builder.generateSubKeys();
  }, {saveOnce: true});

  const inpSubKeyCapUnicodeChange = builder.wrapChange(function () {
    var k = builder.selectedSubKey();
    if (k.length == 0) return;
    const val = builder.fromUnicodeString($(this).val());
    $('.text', k).text(builder.renameSpecialKey(val));
    k.data('text', val);
    $('#inpSubKeyCap').val(val);

    builder.updateCharacterMap(val, true);
    builder.generateSubKeys();
  }, {saveOnce: true});

  $('#inpSubKeyCap')
    .change(inpSubKeyCapChange)
    /*.autocomplete({
      source: builder.specialKeyNames,
      change: inpSubKeyCapChange,
      select: builder.wrapInstant(inpSubKeyCapChange)
    })*/
    .on('input', inpSubKeyCapChange)
    .mouseup(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).focus(function () {
      builder.updateCharacterMap($(this).val(), false);
    }).blur(function () {
      builder.hasSavedSubKeyUndo = false;
    });

  $('#inpSubKeyCapUnicode')
    .change(inpSubKeyCapUnicodeChange)
    .on('input', inpSubKeyCapUnicodeChange)
    .mouseup(function () {
      builder.updateCharacterMap(builder.fromUnicodeString($(this).val()), false);
    }).focus(function () {
      builder.updateCharacterMap(builder.fromUnicodeString($(this).val()), false);
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

  $('#wedgeAddSubKeyLeft').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    builder.selectSubKey(builder.addKey(builder.selectedSubKey().data('type'), 'before-subkey', true));
    builder.generateSubKeys();
  }));

  $('#wedgeAddSubKeyRight').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    builder.selectSubKey(builder.addKey(builder.selectedSubKey().data('type'), 'after-subkey', true));
    builder.generateSubKeys();
  }));

  $('#btnDelSubKey').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    builder.delSelectedSubKey();
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

  $('.btn-add-flick').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    const key = builder.addKey('flick', '#flick');
    const direction = event.target.id.substring(6);  // delete 'flick-' from start of id
    $(key).data('direction', direction).addClass('flick-'+direction);
    builder.selectSubKey(key);
    builder.generateSubKeys();
    builder.addKeyAnnotations(builder.selectedKey());
  }));

  $('#btnAddLongpress').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    builder.selectSubKey(builder.addKey('longpress', '#longpress'));
    builder.generateSubKeys();
    builder.addKeyAnnotations(builder.selectedKey());
  }));

  $('#btnAddMultitap').click(builder.wrapChange(function (event) {
    event.stopPropagation();
    builder.selectSubKey(builder.addKey('multitap', '#multitap'));
    builder.generateSubKeys();
    builder.addKeyAnnotations(builder.selectedKey());
  }));

  this.selectSubKey = function (key) {
    builder.selectedSubKey().removeClass('selected');
    if (key) {
      $(key).addClass('selected');

      let offset = $(key).offset();

      if($(key).data('type') != 'flick') {
        $('.skcontrol.wedge-horz,.skcontrol.wedge-vert').css('display', 'block');
        $('#wedgeAddSubKeyLeft').offset({ left: offset.left - 9, top: offset.top + $(key).outerHeight() + 2 });
        $('#wedgeAddSubKeyRight').offset({ left: offset.left + $(key).outerWidth() - 7, top: offset.top + $(key).outerHeight() + 2 });
      } else {
        $('.skcontrol.wedge-horz,.skcontrol.wedge-vert').css('display', '');
      }

      if(!builder.textControlsInToolbar()) {
        $('input#inpSubKeyCap')
          .css('display', 'block')
          .offset({ left: offset.left + 16, top: offset.top + 4 })
          .width($(key).width() - 32);
      }
      $('div#btnDelSubKey')
        .css('display', 'block')
        .offset({ left: offset.left + $(key).outerWidth() - 14, top: offset.top + 3 });
      this.prepareSubKey();
      builder.enableSubKeyControls();
    } else {
      if(!builder.textControlsInToolbar()) {
        $('input#inpSubKeyCap').css('display', '');
      }
      $('.skcontrol.wedge-horz,.skcontrol.wedge-vert,div#btnDelSubKey').css('display', '');
      builder.enableSubKeyControls();
    }
    builder.saveState();
  }

  this.enableSubKeyControls = function () {
    var key = builder.selectedSubKey();
    if (key.length == 0) {
      $('#subKeyToolbar *').attr('disabled', 'disabled');
    } else {
      $('#subKeyToolbar *').removeAttr('disabled');
      $('#subKeyToolbar #inpSubKeyGestureType').attr('disabled', 'disabled');
    }
  }

  $('#sub-key-container').click(function (event) {
    builder.selectSubKey(null);
  });

}.bind(builder));