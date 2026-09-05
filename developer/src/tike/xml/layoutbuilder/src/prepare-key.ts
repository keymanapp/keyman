/// <reference path="ext/index.d.cts"/>
/// <reference path="ext/jquery-ui/index.d.cts"/>
import { builder } from './builder.js';

builder.addKey = function (type, position, sp) {
  var key = document.createElement('div');
  var ktext = document.createElement('div');
  var khint = document.createElement('div');
  var kid = document.createElement('div');
  var kunderlying = document.createElement('div');
  $(kid).addClass('id');
  $(ktext).addClass('text');
  $(khint).addClass('hint');
  $(kunderlying).addClass('underlying');
  $(key).append(kid);
  $(key).append(ktext);
  $(key).append(khint);
  $(key).append(kunderlying);
  $(key).addClass('key');
  $(key).data('id', 'T_new_' + builder.uniqId);
  $(key).data('type', type);
  builder.uniqId++;
  builder.updateKeyId(key);

  if (type != 'key') {
    $(key).click(function (event) {
      event.stopPropagation();
      builder.selectSubKey(event.currentTarget);
      $(builder.lastFocus ?? '#inpSubKeyCap').focus().select();
    }).dblclick(function (event) {
      event.stopPropagation();
      var layer = $(event.currentTarget).data('nextlayer');
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
      builder.selectKey(event.currentTarget);
      $(builder.lastFocus ?? '#inpKeyCap').focus().select();
    }).dblclick(function (event) {
      event.stopPropagation();
      var layer = $(event.currentTarget).data('nextlayer');
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

  builder.makeKeyDraggable(key);
  builder.formatKey(key, sp);

  return key;
};

builder.formatKey = function (key, sp) {
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

builder.prepareKey = function () {
  var key = builder.selectedKey();
  builder.hasSavedKeyUndo = false;
  let val = $(key).data('text');
  $('#selKeyCapType').val(builder.specialCharacters[val] ? val : '');
  $('#inpKeyCap').val(val);
  $('#inpKeyCapUnicode').val(builder.toUnicodeString(val));

  val = $(key).data('hint');
  ($('#inpKeyHint')[0] as HTMLInputElement).placeholder = builder.inferKeyHintText(null, $(key).data('longpress'), $(key).data('flick'), $(key).data('multitap'));
  $('#inpKeyHint').val(val);
  $('#inpKeyHintUnicode').val(builder.toUnicodeString(val));
  ($('#inpKeyHintUnicode')[0] as HTMLInputElement).placeholder = builder.toUnicodeString(($('#inpKeyHint')[0] as HTMLInputElement).placeholder);

  $('#inpKeyName').val($(key).data('id'));
  $('#inpKeyWidth').val($(key).data('width'));
  $('#inpKeyPadding').val($(key).data('pad'));
  $('#selKeyType').val($(key).data('sp') ? $(key).data('sp') : 0);
  $('#selKeyNextLayer').val($(key).data('nextlayer'));
  $('#selKeyLayerOverride').val($(key).data('layer'));

  builder.makeKeyResizable(key);

  //
  // Prepare presentation of longpress keys, flicks and multitaps
  //

  builder.removeAllSubKeys();
  let longpress = $(key).data('longpress'), flick = $(key).data('flick'), multitap = $(key).data('multitap');
  const hasLongpress = Array.isArray(longpress) && !!longpress.length;
  const hasFlick = Array.isArray(flick) && !!flick.length;
  const hasMultitap = Array.isArray(multitap) && !!multitap.length;

  function addSubKeys(keys, type) {
    for(let key of keys) {
      let nkey = builder.addKey(type, '#'+type);
      let text = builder.inferKeyText(key.text, key.id);

      if(key.direction) $(nkey).addClass('flick-'+key.direction);

      $(nkey)
        .data('text', key.text)
        .data('id', key.id)
        .data('sp', key.sp)
        .data('font', key.font)
        .data('fontsize', key.fontsize)
        .data('nextlayer', key.nextlayer)
        .data('layer', key.layer)
        .data('direction', key.direction) // used only by flicks
        .data('default', key.default) // used only by longpress
        .css('width', (100 * 0.7) + 'px')
        .css('font-family',builder.escapeFontName(key.font))
        .css('font-size', key.fontsize);

      if(builder.specialCharacters[text]) {
        $(nkey).addClass('key-special-text');
      }

      if(key.default) {
        $(nkey).addClass('key-is-default');
      }

      $('.text', nkey).text(builder.renameSpecialKey(text));
      builder.updateKeyId(nkey);
    }
  }

  if(hasLongpress) addSubKeys(longpress, 'longpress');
  if(hasFlick) addSubKeys(flick, 'flick');
  if(hasMultitap) addSubKeys(multitap, 'multitap');
  builder.addKeyAnnotations(key);
  builder.prepareSubKey();
  builder.enableKeyControls();
  builder.enableSubKeyControls();
}

builder.makeKeyResizable = function(key) {
  $(key).resizable({
    minWidth: 10 * builder.xscale,
    minHeight: 100 * builder.yscale,
    maxWidth: 500 * builder.xscale,
    maxHeight: 100 * builder.yscale,
    ghost: false,
    handles: "e",
    resize: function (_event, ui) {
      let newWidth = Math.round(ui.size.width / builder.xscale);
      builder.selectedKey().data('width', newWidth);
      $('#inpKeyWidth').val(newWidth);
      builder.updateKeySizeInfo();
    },
    start: function (_event, ui) {
      builder.saveUndo();
      $(ui.originalElement).css('position', 'relative');
      $(ui.originalElement).css('left', '');
    },
    stop: function (_event, _ui) {
      builder.rescale();
      builder.generate();
    }
  });
}
