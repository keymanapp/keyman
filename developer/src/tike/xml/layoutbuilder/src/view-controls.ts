/// <reference path="ext/index.d.cts"/>
/// <reference path="ext/jquery-ui/index.d.cts"/>
import { builder } from './builder.js';

$('#btnViewOptions').click(function () {
  ($('#chkShowAllModifierOptions')[0] as HTMLInputElement).checked = builder.showAllModifierCombinations;
  $('#viewOptionsDialog').dialog('open');
});

$('#chkShowAllModifierOptions').click(function (event) {
  event.stopImmediatePropagation();
  builder.showAllModifierCombinations = ($('#chkShowAllModifierOptions')[0] as HTMLInputElement).checked;
  builder.fillModifierSelect();
  builder.prepareKey();
});

$('#viewOptionsForm').on('submit', function() {
  $('#viewOptionsDialog').dialog('close');
  return false;
});

$('#viewOptionsDialog').dialog({
  autoOpen: false,
  height: 300,
  width: 350,
  modal: true,
  buttons: {
    "Close": function () {
      $(this).dialog('close');
    }
  }
});
