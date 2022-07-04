$(function() {
  // Key drag and drop functionality

  builder.makeKeyDraggable = function(key) {
    const selectedKeyType = $(key).data('type');
    if(!selectedKeyType.match(/^(key|longpress|multitap)$/)) return;

    $(key).draggable({
      revert: "invalid",
      stack: ".key",
      zIndex: 100,
      helper: "clone",
      start: function (event, ui) {
        const elements =
          selectedKeyType == 'key' ? ['#kbd', '.row'] :
          selectedKeyType == 'longpress' ? ['#longpress', '#longpress'] :
          /* selectedKeyType == 'multitap' */ ['#multitap', '#multitap'];

        if (selectedKeyType == 'key') {
          builder.selectKey(null, false);
        } else {
          builder.selectSubKey(null);
        }

        var drag = this;
        drag.overList = [];
        var pos = $(this).position();
        $(this).addClass('key-dragging');
        $(elements[0] + ' .key').before(function (index) {
          if (this == drag) return '';
          if ($(this).prev()[0] == drag) return '<div class="key-droppable key-current"></div>';
          return '<div class="key-droppable"></div>';
        });
        $(elements[1]).append('<div class="key-droppable"></div>');

        $('.key-droppable').css('margin-top', $(this).css('margin-top')).css('height', $(this).css('height')).droppable({
          accept: ".key",
          tolerance: "touch",
          over: function (_event, _ui) {
            drag.overList.push(this);
            $(drag.overList[0]).addClass('key-droppable-hover');
          },
          out: function (_event, _ui) {
            var n = drag.overList.indexOf(this);
            if (n >= 0)
              drag.overList.splice(n, 1);
            $(this).removeClass('key-droppable-hover');
            if (drag.overList.length > 0) $(drag.overList[0]).addClass('key-droppable-hover');
          },
          drop: function (_event, _ui) {
            //
            // Drop the selected key into its new position
            //
            builder.saveUndo();
            $(drag).detach().removeClass('key-dragging');
            $(drag.overList[0]).after(drag);
            if (selectedKeyType == 'key') builder.rescale(); else builder.generateSubKeys();
            builder.generate();
          }
        });
        $('.key-current').css('width', $(drag).width() + 'px').css('margin-left', -($(drag).width()) + 'px');
      },
      stop: function (_event, _ui) {
        $('.key-droppable').remove();
        $(this).removeClass('key-dragging');
      }
    });
  }
}.bind(builder));