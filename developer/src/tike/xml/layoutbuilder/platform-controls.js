$(function() {
  $('#btnAddPlatform').click(function () {
    $('#selAddPlatform option').remove();
    var platforms = { 'tablet': 1, 'desktop': 1, 'phone': 1 };
    for (var platform in KVKL) {
      platforms[platform] = 0;
    }
    let nPlatforms = 0;
    for (platform in platforms) {
      if (platforms[platform]) {
        var opt = document.createElement('option');
        $(opt).text(platform);
        $('#selAddPlatform').append(opt);
        nPlatforms++;
      }
    }
    if(nPlatforms == 0) {
      $('#addPlatformDialogNoPlatformsToAdd').dialog('open')
    } else {
      $('#addPlatformDialog').dialog('open')
    }
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
    $('#selDefaultHint').val(KVKL[builder.lastPlatform].defaultHint ?? 'dot');
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

  //
  // Platform dialog -- no platforms to add
  //

  $('#addPlatformDialogNoPlatformsToAdd').dialog({
    autoOpen: false,
    height: 150,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {
        $(this).dialog('close');
      }
    }
  });

  //
  // Platform Properties Dialog
  //

  $('#chkDisplayUnderlying').click(function (event) {
    event.stopImmediatePropagation();
    let selection = builder.saveSelection();
    builder.saveUndo();
    KVKL[builder.lastPlatform].displayUnderlying = $('#chkDisplayUnderlying')[0].checked;
    builder.generate();
    builder.prepareLayer();
    builder.restoreSelection(selection);
  });

  $('#selDefaultHint').change(function () {
    let selection = builder.saveSelection();
    builder.saveUndo();
    KVKL[builder.lastPlatform].defaultHint = $('#selDefaultHint').val();
    if(KVKL[builder.lastPlatform].defaultHint == 'dot') {
      delete KVKL[builder.lastPlatform].defaultHint;
    }
    builder.generate();
    builder.prepareLayer();
    builder.restoreSelection(selection);
  });

  $('#platformPropertiesDialog').dialog({
    autoOpen: false,
    height: 400,
    width: 450,
    modal: true,
    buttons: {
      "Close": function () {
        $(this).dialog('close');
      }
    }
  });

}.bind(builder));