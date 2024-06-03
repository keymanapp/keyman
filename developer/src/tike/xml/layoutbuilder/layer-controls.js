$(function() {

  $('#addLayerName').on('input', function() {
    var layerName = $(this).val();
    $('#addLayerList').val(layerName);
    if($('#addLayerList')[0].selectedIndex < 0) {
      $('#addLayerList').val('(custom)');
      $('#addLayerNote').text('');
    } else {
      $('#addLayerNote').text(layerName+' is a recognised modifier-aware layer name.');
    }
  });

  $('#addLayerList').change(function() {
    var v = $(this).val();
    if(v == '(custom)') {
      $('#addLayerName').val('');
      $('#addLayerNote').text('');
    } else {
      $('#addLayerName').val(v);
      $('#addLayerNote').text(v+' is a recognised modifier-aware layer name.');
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
    builder.generate(false,true);
  });

  $('#btnEditLayer').click(function () {
    $('#layerName').val(KVKL[builder.lastPlatform].layer[builder.lastLayerIndex].id);
    $('#layerPropertiesDialog').dialog('open');
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
        if (!newLayerName.match(/^[a-zA-Z0-9_-]+$/)) {
          alert('Layer name must contain only alphanumerics, underscore and hyphen.');
          return;
        }

        builder.saveUndo();
        builder.generate();

        var platform = KVKL[builder.lastPlatform];
        var oldLayerName = platform.layer[builder.lastLayerIndex].id;

        let fixup = function(key) {
          if (key.layer == oldLayerName) {
            key.layer = newLayerName;
          }
          if (key.nextlayer == oldLayerName) {
            key.nextlayer = newLayerName;
          }
        }

        platform.layer.forEach(layer => {
          if (layer.row) {
            layer.row.forEach(row => {
              if (row.key) {
                row.key.forEach(key => {
                  fixup(key);

                  if (key.sk) {
                    key.sk.forEach(k => fixup(k));
                  }
                  if (key.flick) {
                    key.flick.forEach(k => fixup(k));
                  }
                  if (key.multitap) {
                    key.multitap.forEach(k => fixup(k));
                  }
                });
              }
            });
          }
        });

        platform.layer[builder.lastLayerIndex].id = newLayerName;
        builder.prepareLayers();
        $('#selLayer').val(builder.lastLayerIndex);
        builder.selectLayer();
        builder.generate();
        $(this).dialog('close');
      },
      "Cancel": function () {
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
        var id = $('#addLayerName').val();
        if (!id.match(/^[a-zA-Z0-9_-]+$/)) {
          alert('Layer name must contain only alphanumerics, underscore and hyphen.');
          return;
        }
        for(var i = 0; i < KVKL[builder.lastPlatform].layer.length; i++) {
          if(KVKL[builder.lastPlatform].layer[i].id == id) {
            alert('Layer name must not already be in use for the current platform.');
            return;
          }
        }
        builder.saveUndo();
        var layer = $.extend(true, {}, KVKL[builder.lastPlatform].layer[builder.lastLayerIndex]);
        layer.id = id;
        var n = KVKL[builder.lastPlatform].layer.push(layer) - 1;
        builder.selectPlatform();
        $('#selLayer').val(n);
        builder.selectLayer();
        builder.generate(false,true);
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

  $('#selLayer').change(function () {
    if (builder.lastPlatform) builder.generate(false,false);
    let selection = builder.saveSelection();
    builder.selectLayer();
    builder.restoreSelection(selection);
    builder.saveState();
  });

}.bind(builder));
