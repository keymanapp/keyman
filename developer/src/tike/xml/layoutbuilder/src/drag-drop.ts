/// <reference path="../../../../../../node_modules/@types/jquery/index.d.ts"/>
/// <reference path="../../../../../../node_modules/@types/jqueryui/index.d.ts"/>
import { builder } from './builder.js';

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

      var drag = event.currentTarget;
      builder.overList = [];
      $(drag).addClass('key-dragging');
      const elemKey = $(elements[0] + ' .key');
      elemKey.before(function (index) {
        if (elemKey[index] == drag) return '';
        if ($(elemKey[index]).prev()[0] == drag) return '<div class="key-droppable key-current"></div>';
        return '<div class="key-droppable"></div>';
      });
      $(elements[1]).append('<div class="key-droppable"></div>');

      $('.key-droppable').css('margin-top', $(drag).css('margin-top')).css('height', $(drag).css('height')).droppable({
        accept: ".key",
        tolerance: "touch",
        over: function (event,) {
          builder.overList.push(event.target);
          $(builder.overList[0]).addClass('key-droppable-hover');
        },
        out: function (event) {
          var n = builder.overList.indexOf(event.target);
          if (n >= 0)
            builder.overList.splice(n, 1);
          $(event.target).removeClass('key-droppable-hover');
          if (builder.overList.length > 0) $(builder.overList[0]).addClass('key-droppable-hover');
        },
        drop: function () {
          //
          // Drop the selected key into its new position
          //
          builder.saveUndo();
          $(drag).detach().removeClass('key-dragging');
          $(builder.overList[0]).after(drag);
          if (selectedKeyType == 'key') builder.rescale(); else builder.generateSubKeys();
          builder.generate();
        }
      });
      $('.key-current').css('width', $(drag).width() + 'px').css('margin-left', -($(drag).width()) + 'px');
    },
    stop: function (event) {
      $('.key-droppable').remove();
      $(event.currentTarget).removeClass('key-dragging');
    }
  });
}
