$(function() {

  this.undoStack = [];
  this.redoStack = [];


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

    const lastPresentation = $('#selPlatformPresentation').val();

    builder.preparePlatforms();
    builder.enableUndoControls();

    $('#selPlatform').val(s.platform);
    builder.selectPlatform();
    $('#selLayer').val(s.layer);
    builder.selectLayer();
    builder.selectKey($('#kbd .key').filter(function (index) { return $(this).data('id') === s.key; }).first());
    if (s.subkey) builder.selectSubKey($('#sub-key-groups .key').filter(function (index) { return $(this).data('id') === s.subkey; }).first());

    // If we are on the same platform, restore the user's current presentation,
    // which has been reset by the earlier `preparePlatforms()` call. We can't
    // manage this across platforms because the presentation values are unique
    // per platform
    if($(`#selPlatformPresentation option[value="${lastPresentation}"]`).length) {
      $('#selPlatformPresentation').val(lastPresentation);
      builder.selPlatformPresentationChange();
    }
  }

  this.saveUndo = function (saveToRedo) {
    if (!saveToRedo) {
      builder.redoStack = [];
    }
    builder.generate(true,false);
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

  $('#btnUndo').click(function () {
    builder.undo();
  });

  $('#btnRedo').click(function () {
    builder.redo();
  });

}.bind(builder));
