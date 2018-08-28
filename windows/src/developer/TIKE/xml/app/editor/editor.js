window.editorGlobalContext = {
  loading: false
};

(function(context) {
  var editor = null;
  var errorRange = null;
  var ranges = [];
  let params = (new URL(location)).searchParams;
  let filename = params.get('filename');
  let mode = params.get('mode');
  
  if(!mode) {
    mode = 'keyman';
  }
  
  /* Search and replace interfaces */
  
  context.searchFind = function() {  
    editor.commands.byName.find.exec(editor);
  };

  context.searchFindNext = function() {  
    editor.commands.byName.selectOrFindNext.exec(editor);
  };

  context.searchReplace = function() {  
    editor.commands.byName.replace.exec(editor);
  };
  
  /* Edit command interfaces */

  context.editUndo = function() {
    editor.undo();
  };

  context.editRedo = function() {
    editor.redo();
  };
  
  context.editSelectAll = function() {
    editor.selectAll();
  };

  /* Row selection */

  context.moveCursor = function (o) {
    editor.moveCursorTo(o.row, o.column);
    editor.renderer.scrollCursorIntoView();
  };
  
  /* Error highlighting */
  
  context.setRowColor = function(o) {
    if (o.style) {
      var r = new ace.Range(o.row, 0, o.row, Infinity);
      r.row = o.row;
      r.id = editor.session.addMarker(r, 'km_'+o.style, "fullLine", false);
      ranges.push(r);
    } else {
      ranges = ranges.filter(function (v) {
        if (v.row !== o.row) {
          return true;
        }
        editor.session.removeMarker(v.id);
        return false;
      });
    }
    editor.moveCursorTo(o.row, 0);
    editor.selection.clearSelection();
    editor.renderer.scrollCursorIntoView();
  };
  
  context.highlightError = function(row) {
    if(errorRange) {
      editor.session.removeMarker(errorRange.id);
    }
    if(typeof row == 'number') {
      errorRange = new ace.Range(row, 0, row, Infinity);
      errorRange.id = editor.session.addMarker(errorRange, 'km_error', "fullLine", false);
    }
  };

  /* Printing */

  context.print = function () {
    require("ace/config").loadModule("ace/ext/static_highlight", function (m) {
      var result = m.renderSync(
        editor.getValue(), editor.session.getMode(), editor.renderer.theme
      );
      var iframe = document.createElement('iframe');
      iframe.onload = function () {
        iframe.contentWindow.document.open();
        iframe.contentWindow.document.write(result.html);
        iframe.contentWindow.document.close();
        var s = iframe.contentWindow.document.createElement('style');
        s.type = 'text/css';
        s.appendChild(iframe.contentWindow.document.createTextNode(result.css));
        iframe.contentWindow.document.head.appendChild(s);
        // TODO: Add page setup -- paper size, margins
        window.setTimeout(function () {
          iframe.contentWindow.print();
          document.body.removeChild(iframe);
        }, 10);
      };
      document.body.appendChild(iframe);
    });
  };
      
  /**
    Notifies the host application of an event or command from the
    text editor. Commands are cached until idle to allow for batches
    of commands to be sent together.
    
    Copy of this is in builder.js (ready for refactor!)
  */
  var commands = [];

  var command = function (cmd) {
    if (navigator.userAgent.indexOf('TIKE') < 0) {
      // Don't execute command if not in the correct host application (allows for browser-based testing)
      return false;
    }
    // queue commands to build a single portmanteau command when we return to idle
    if (commands.length == 0) {
      window.setTimeout(function() {
        try {
          location.href = 'keyman:command?' + commands.reduce(function(a,v) { return a + '&' + v; });
          commands = [];
        } catch (e) {
          // ignore errors - saves weirdness happening when testing outside TIKE.exe
        }
      }, 10);
    }
    commands.push(cmd);
  };
  
  /**
  */
  var updateState = function() {
    command(editor.getSelectedText() == '' ? 'no-selection' : 'has-selection');
    command(editor.session.getUndoManager().hasUndo() ? 'undo-enable' : 'undo-disable');
    command(editor.session.getUndoManager().hasRedo() ? 'redo-enable' : 'redo-disable');
    let c = editor.selection.getCursor();
    command('location,'+c.row+','+c.column);
    command('insert-mode,'+(editor.session.getOption('overwrite') ? 'Overwrite' : 'Insert'));
//    command(editor.session.getUndoManager().isClean() ? 'modified' : 'not-modified');
    var s = getTokenAtCursor();
    if(s) {
      command('token,'+s.column+','+encodeURIComponent(s.text));
    }
  };
  
  var getTokenAtCursor = function() {
    var txt = editor.getSelectedText();
    if(txt != '') {
      // We'll always return the first 100 characters of the selection and not 
      // do any manipulation here.
      return {column:null,text:txt.substr(0, 99)};
    }
    
    if(mode == 'keyman') {
      // Get the token under the cursor
      var c = editor.session.selection.getCursor();
      var line = editor.session.getLine(c.row);
      return {column:c.column, text:line};
    } else {
      // Get the character under the cursor
      return null;
    }
  };
  
  /**
    Initialize the editor
  */
  $(function initialize() {
    editor = ace.edit("editor");

    // Remove Alt+ key bindings which may conflict with environment shortcuts, e.g. Alt+E, function keys

    var reAltKeyBindings = /(^alt-[a-z0-9]|f[0-9]+)$/;
    for (var key in editor.keyBinding.$defaultHandler.commandKeyBinding) {
      if (key.match(reAltKeyBindings)) {
        delete editor.keyBinding.$defaultHandler.commandKeyBinding[key];
      }
    }

    editor.setTheme("ace/theme/xcode");
    
    editor.session.selection.on('changeCursor', updateState);    
    editor.session.on('changeOverwrite', updateState);
    editor.session.selection.on('changeSelection', updateState);
    
    editor.session.on('change', function(delta) {
      if(!context.loading) {
        $.post("/app/source/file", {
          Filename: filename,
          Data: editor.session.getValue()
          // delta.start, delta.end, delta.lines, delta.action
        });
        context.highlightError();//clear the selected error
        command('modified');
        updateState();
      }
    });
    // Start reading the source file
    $.get("/app/source/file", { 
      Filename: filename
    }, function (response) {
      context.loading = true;
      editor.session.setValue(response);
      editor.session.setMode("ace/mode/"+mode);
      context.loading = false;
    }, 
    "text"); 
  });
})(window.editorGlobalContext);