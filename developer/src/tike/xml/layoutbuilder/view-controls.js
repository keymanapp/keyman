$(function() {

  $('#btnViewOptions').click(function () {
    $('#chkShowAllModifierOptions')[0].checked = builder.showAllModifierCombinations;
    $('#viewOptionsDialog').dialog('open');
  });

  $('#viewOptionsDialog').dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "OK": function () {
        builder.showAllModifierCombinations = $('#chkShowAllModifierOptions')[0].checked;
        builder.fillModifierSelect();
        builder.prepareKey();
        $(this).dialog('close');
      },
      "Cancel": function () {
        $(this).dialog('close');
      }
    }

  });

}.bind(builder));
