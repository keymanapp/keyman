/*
 * Editor using monaco
 *
 * Helpful references:
 * https://microsoft.github.io/monaco-editor/api/index.html
 * https://blog.expo.io/building-a-code-editor-with-monaco-f84b3a06deaf
 */

window.editorGlobalContext = {
  loading: false
};

async function loadSettings() {
  return await $.getJSON('/app/settings/editor');
}

(function(context) {
  var editor = null;
  var errorRange = null;
  var executionPoint = [];
  var breakpoints = [];
  var fontCss = null;
  let params = (new URL(location)).searchParams;
  let filename = params.get('filename');
  let mode = params.get('mode');
  let isWordlistTsv = false;

  if(!mode) {
    mode = 'keyman';
  }

  /**
    Initialize the editor
  */

  require.config({ paths: {
    'vs': '../lib/monaco/min/vs'
  } });

  require(['vs/editor/editor.main','language.keyman'], function (_editor, _language) {

    //
    // Register Keyman .kmn tokens provider and language formatter
    // https://github.com/Microsoft/monaco-editor/blob/master/test/playground.generated/extending-language-services-custom-languages.html
    //

    monaco.languages.register({ id: 'keyman' });
    monaco.languages.setMonarchTokensProvider('keyman', _language.language);

    //
    // Create editor and load source file
    //

    isWordlistTsv = (mode == 'wordlisttsv');
    if(isWordlistTsv) {
      mode = 'text';
    }

    // handle ctrl+click to open new window by passing it back to TIKE
    const openerService = {
      open: function (resource, _options) {
        window.open(resource.toString(), '_blank');
      }
    };

    editor = monaco.editor.create(document.getElementById('editor'), {
      language: mode,
      minimap: {
        enabled: false
      },
      glyphMargin: true,
      lineNumbersMinChars: 2,
      disableMonospaceOptimizations: true
    }, {
      openerService
    });

    $.get("/app/source/file",
      {
        Filename: filename
      },
      function (response) {
        context.loading = true;
        editor.setValue(response);
        editor.focus();
        context.loading = false;
      },
      "text"
    );

    //
    // Set initial fonts
    //

    context.setFonts({
      codeFont: { name: params.get('codeFontName'), size: params.get('codeFontSize') },
      charFont: { name: params.get('charFontName'), size: params.get('charFontSize') }
    });

    //
    // Setup callbacks
    //

    window.addEventListener('resize', function() {
      editor.layout();
    });

    const model = editor.getModel();

    if(isWordlistTsv) {
      model.updateOptions({insertSpaces: false});
    }

    context.reloadSettings();

    model.onDidChangeContent(() => {
      // Even when loading, we post back the data to the backend so we have an original version
      $.post("/app/source/file", {
        Filename: filename,
        Data: model.getValue()
        // delta.start, delta.end, delta.lines, delta.action
      });
      if (!context.loading) {
        context.highlightError(); // clear the selected error
        command('modified');
        updateState();
      }
    });

    editor.onDidChangeCursorPosition(updateState);
    editor.onDidChangeCursorSelection(updateState);
    editor.onMouseDown(e => {
      if (e.target.type !== monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN) {
        return;
      }
      command('breakpoint-clicked,'+(e.target.position.lineNumber-1));
    });
  });

  //
  // Search and replace interfaces
  //

  context.searchFind = function () {
    editor.trigger('', 'actions.find');
  };

  context.searchFindNext = function() {
    editor.trigger('', 'editor.action.nextMatchFindAction');
  };

  context.searchReplace = function() {
    editor.trigger('', 'editor.action.startFindReplaceAction');
  };

  //
  // Edit command interfaces
  //

  context.editUndo = function() {
    editor.getModel().undo();
  };

  context.editRedo = function() {
    editor.getModel().redo();
  };

  context.editSelectAll = function() {
    editor.setSelection(editor.getModel().getFullModelRange());
  };

  //
  // Row selection
  //

  context.moveCursor = function (o) {
    editor.setPosition({ column: o.column + 1, lineNumber: o.row + 1 });
    editor.revealPositionInCenterIfOutsideViewport(editor.getPosition(), monaco.editor.ScrollType.Smooth);
  };

  //
  // Debug interactions
  //

  context.setBreakpoint = function (row) {
    if (!breakpoints[row]) {
      breakpoints[row] = editor.deltaDecorations(
        [],
        [{ range: new monaco.Range(row + 1, 1, row + 1, 1), options: { isWholeLine: true, glyphMarginClassName: 'km_breakpoint' } }]
      );
    }
  };

  context.clearBreakpoint = function (row) {
    if (breakpoints[row]) {
      editor.deltaDecorations(breakpoints[row], []);
      breakpoints[row] = null;
    }
  };

  context.updateExecutionPoint = function (row) {
    if(!editor) {
      // At debugger startup time, it initializes asynchronously at the same
      // time as the editor, which means that in some circumstances the
      // debugger will call to SetExecutionPointLine before the editor has
      // loaded. The safest thing to do here is just exit. #11586
      return;
    }
    if(row >= 0) {
      executionPoint = editor.deltaDecorations(
        executionPoint,
        [{ range: new monaco.Range(row + 1, 1, row + 1, 1), options: { isWholeLine: true, linesDecorationsClassName: 'km_executionPoint' } }]
      );
      context.moveCursor({ row: row, column: 0 });
    } else {
      executionPoint = editor.deltaDecorations(executionPoint, []);
    }
  };

  //
  // Character map drag+drop and double-click insertion
  //

  context.charmapDragOver = function(o) {

    // Convert X, Y to document coordinates

    let target = editor.getTargetAtClientPoint(o.x, o.y);

    if(target === null || target.type !== monaco.editor.MouseTargetType.CONTENT_TEXT) {
      return false;
    }

    // Move insertion point accordingly

    let position = editor.getPosition();

    if(!position.equals(target.position)) {
      editor.setPosition(target.position);
    }

    return true;
  };

  context.charmapDragDrop = function(o) {

    // Convert X, Y to document coordinates

    if(o.x >= 0 && o.y >= 0) {
      let target = editor.getTargetAtClientPoint(o.x, o.y);

      if(target === null || target.type !== monaco.editor.MouseTargetType.CONTENT_TEXT) {
        return false;
      }

      editor.setPosition(target.position);
    }

    editor.trigger('keyboard', 'type', {text: o.text});
  };

  //
  // Error highlighting
  //

  context.highlightError = function (row) {
    if (errorRange) {
      editor.deltaDecorations(errorRange, []);
      errorRange = null;
    }
    if (typeof row == 'number') {
      errorRange = editor.deltaDecorations([], [{ range: new monaco.Range(row + 1, 1, row + 1, 1), options: { isWholeLine: true, linesDecorationsClassName: 'km_error', className: 'km_error' } }]);
    }
  };

  context.replaceSelection = function (o) {
    let r = new monaco.Range(o.top + 1, o.left + 1, o.bottom + 1, o.right + 1);
    editor.setSelection(r);
    editor.executeEdits("", [
      { range: r, text: o.newText }
    ]);
  };

  context.setText = function (text) {
    let range = editor.getSelection();
    context.loading = true;
    editor.setValue(text);
    editor.setSelection(range);
    context.loading = false;
  };

  //
  // Set character and code fonts
  //

  context.setFonts = function (fonts) {
    if (fonts == null) {
      return false;
    }

    if (!fontCss) {
      fontCss = document.createElement('style');
      document.head.appendChild(fontCss);
    }

    if(mode == 'keyman') {
      fontCss.innerHTML = ".mtk20, .mtk8 { font-size: " + fonts.charFont.size + "px; font-family: \"" + fonts.charFont.name + "\"; }";
    } else if(mode == 'xml') {
      // This is very crude but good enough for now. It is not 100% because we
      // still have some font styling bleeding into wrong areas, e.g. <element
      // id="foo"> x </element> will have `"foo">` all as a single span, so the
      // `>` ends up with our character font.
      fontCss.innerHTML =
        ".mtk6 + .mtk1, " + // text on same line as >{mtk6}
        "div.view-line > span > .mtk1, " + // text on new line
        ".mtk4 + .mtk1 + .mtk6, " + // xml attributes, id{mtk4} ={mtk1} "value"{mtk6}
        ".mtk6 + .mtk6" + // long attributes
        "{ font-size: " + fonts.charFont.size + "px; font-family: \"" + fonts.charFont.name + "\"; }";
    } else if(mode == 'json') {
      fontCss.innerHTML = ".mtk5 { font-size: " + fonts.charFont.size + "px; font-family: \"" + fonts.charFont.name + "\"; }";
    } else {
      fontCss.innerHTML = ".mtk1 { font-size: " + fonts.charFont.size + "px; font-family: \"" + fonts.charFont.name + "\"; }";
    }

    // Calculate the appropriate line height based on the maximum from the two fonts set

    var eChar = document.createElement('div');
    eChar.innerHTML = '?';
    eChar.style = "position:absolute; top: -50000px; left: 0; font-size: " + fonts.charFont.size + "px; font-family: \"" + fonts.charFont.name + "\";";
    document.body.appendChild(eChar);
    var lineHeight = eChar.offsetHeight;

    eChar.style = "position:absolute; top: -50000px; left: 0; font-size: " + fonts.codeFont.size + "px; font-family: \"" + fonts.codeFont.name + "\";";
    lineHeight = Math.max(lineHeight, eChar.offsetHeight);

    document.body.removeChild(eChar);

    editor.updateOptions({
      fontFamily: fonts.codeFont.name,
      fontSize: fonts.codeFont.size,
      lineHeight: lineHeight
    });
  };

  //
  // Themes and settings
  //

  context.reloadSettings = function () {
    loadSettings().then(function(_settings) {
      _settings = $.extend({
        useTabChar: false,
        indentSize: 4
      }, _settings);

      //
      // Define a custom theme if specified in the settings
      //

      var themeName = 'vs';

      if(_settings.theme) {
        if(typeof _settings.theme == 'string') {
          themeName = _settings.theme;
        } else {
          themeName = 'keyman-custom';
          monaco.editor.defineTheme(themeName, _settings.theme);
        }
      }

      monaco.editor.setTheme(themeName);

      editor.getModel().updateOptions({
        insertSpaces:
          !_settings.useTabChar && // user pref
          !isWordlistTsv, // We always use tabs for TSV files
        tabSize: _settings.indentSize
      });
    });
  }


  //
  // Printing
  //

  context.print = function () {
    /****require("ace/config").loadModule("ace/ext/static_highlight", function (m) {
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
    });*/
  };

  /**
    Notifies the host application of an event or command from the
    text editor. Commands are cached until idle to allow for batches
    of commands to be sent together.

    Copy of this is in builder.js (ready for refactor!)
  */
  var commands = [];

  var command = function (cmd) {
    if (navigator.userAgent.indexOf('Keyman') < 0) {
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

  //
  // Updates the state of the host application
  //

  var updateState = function () {
    let s = editor.getSelection();
    command(s.isEmpty() ? 'no-selection' : 'has-selection');
    command(editor.getModel().canUndo() ? 'undo-enable' : 'undo-disable');
    command(editor.getModel().canRedo() ? 'redo-enable' : 'redo-disable');

    var n = editor.getModel().getValueInRange(s).length;
    if (editor.getSelection().getDirection() == monaco.SelectionDirection.LTR) n = -n;
    command('location,' + (s.startLineNumber-1) + ',' + (s.startColumn-1) + ',' + (s.endLineNumber-1) + ',' + (s.endColumn-1) + ',' + n);
    var token = getTokenAtCursor();
    if (token) {
      let text;
      try {
        text = encodeURIComponent(token.text);
      } catch(e) {
        if(e instanceof URIError) {
          // if token.text contains an unpaired surrogate, encodeURIComponent
          // fails with a URIError, in which case we will just avoid
          // sending the token command.
          return;
        }
        throw e;
      }
      command('token,' + token.column + ',' + text);
    }
  };

  var getTokenAtCursor = function () {
    var txt = editor.getModel().getValueInRange(editor.getSelection());
    if (txt != '') {
      // We'll always return the first 100 characters of the selection and not
      // do any manipulation here.
      return { column: null, text: txt.substr(0, 99) };
    }

    if (mode == 'keyman') {
      // Get the token under the cursor
      var c = editor.getSelection();
      try {
        var line = editor.getModel().getLineContent(c.positionLineNumber);
      } catch(e) {
        // In some situations, e.g. deleting a selection at the end of the document,
        // the selected line may be past the end of the document for a moment
        return null;
      }
      return { column: c.positionColumn-1, text: line };
    } else {
      // Get the character under the cursor
      return null;
    }
  };
})(window.editorGlobalContext);