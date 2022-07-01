$(function() {

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
        builder.post();
      }
    });

    //
    // Prepare sub-key array
    //

    builder.removeAllSubKeys();
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
        .data('key-type', 'longpress')
        .data('text', text)
        .data('id', key.id)
        .data('sp', key.sp)
        .data('font', key.font)
        .data('fontsize', key.fontsize)
        .data('nextlayer', key.nextlayer)
        .data('layer', key.layer)
        .css('width', (100 * 0.7) + 'px')
        .css('font-family', KVKL[builder.lastPlatform].font);

      if(this.specialCharacters[text])
        $(nkey).addClass('key-special-text');

      $('.text', nkey).text(this.renameSpecialKey(text));
      builder.updateKeyId(nkey);
    }
    builder.prepareSubKey();
    builder.enableSubKeyControls();
  }
}.bind(builder));