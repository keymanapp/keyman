$(function() {

  $('#btnViewOptions').click(function () {
    $('#chkShowAllModifierOptions')[0].checked = builder.showAllModifierCombinations;
    $('#viewOptionsDialog').dialog('open');
  });

  $('#chkShowAllModifierOptions').click(function (event) {
    event.stopImmediatePropagation();
    builder.showAllModifierCombinations = $('#chkShowAllModifierOptions')[0].checked;
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

}.bind(builder));
