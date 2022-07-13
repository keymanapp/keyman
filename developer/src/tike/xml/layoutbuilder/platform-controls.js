$(function() {
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
    builder.generate(false,true);
  });

  $('#selPlatform').change(function () {
    if (builder.lastPlatform) builder.generate(false,false);
    let selection = builder.saveSelection();
    builder.selectPlatform();
    builder.restoreSelection(selection);
    builder.saveState();
  });

  $('#btnEditPlatform').click(function () {
    $('#chkDisplayUnderlying')[0].checked = KVKL[builder.lastPlatform].displayUnderlying;
    $('#selDisplayHint').val(KVKL[builder.lastPlatform].displayHint ?? 'dot');
    $('#platformPropertiesDialog').dialog('open');
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

        var platform = $('#selAddPlatform').val();
        KVKL[platform] = $.extend(true, {}, KVKL[builder.lastPlatform]);  // copy existing platform

        builder.preparePlatforms();
        $('#selPlatform').val(platform);
        builder.selectPlatform();

        builder.generate(false,true);

        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }
  });

  $('#platformPropertiesDialog').dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {

        KVKL[builder.lastPlatform].displayUnderlying = $('#chkDisplayUnderlying')[0].checked;
        KVKL[builder.lastPlatform].displayHint = $('#selDisplayHint').val();
        if(KVKL[builder.lastPlatform].displayHint == 'dot') {
          delete KVKL[builder.lastPlatform].displayHint;
        }

        let selection = builder.saveSelection();
        builder.saveUndo();
        builder.generate();
        builder.prepareLayer();
        builder.restoreSelection(selection);

        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }
  });

}.bind(builder));