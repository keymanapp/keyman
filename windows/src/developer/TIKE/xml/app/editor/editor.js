window.editorGlobalContext = {
  loading: false
};

(function(context) {
  var editor = null;
  var errorRange = null;
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
  
  /* Error highlighting */
  
  context.highlightError = function(row) {
    if(errorRange) {
      editor.session.removeMarker(errorRange.id);
    }
    if(typeof row == 'number') {
      errorRange = new ace.Range(row, 0, row, Infinity);
      errorRange.id = editor.session.addMarker(errorRange, 'km_error', "fullLine", false);
    }
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
    command('location,Line '+(c.row+1)+', Col '+(c.column+1));
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