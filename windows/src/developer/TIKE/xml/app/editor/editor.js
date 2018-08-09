window.editorGlobalContext = {
  loading: false
};

(function(context) {
  var editor = null;
  var filename = decodeURIComponent(location.search.substr(1));
  
  // Initialize the editor
  $(function() {
    editor = ace.edit("editor");
    editor.setTheme("ace/theme/monokai");
    editor.session.setMode("ace/mode/keyman");
    editor.session.on('change', function(delta) {
      if(!context.loading) {
        $.post("/app/source/file", {
          Filename: filename,
          Data: editor.session.getValue()
          // delta.start, delta.end, delta.lines, delta.action
        });
        location.href = 'keyman:command?modified';
      }
    });
    // Start reading the source file
    $.get("/app/source/file", { 
      Filename: filename
    }, function (response) {
      context.loading = true;
      editor.session.setValue(response);
      context.loading = false;
    }, 
    "text"); 
  });
})(window.editorGlobalContext);