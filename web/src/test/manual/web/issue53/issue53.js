function loadKeyboards()
{
  // Add a fully-specified, locally-sourced, keyboard.
  keyman.addKeyboards({id:'layered_debug_keyboard',name:'Web_Layer_Debugging',
    languages:{
      id:'dbg',name:'Debug',region:'North America'
      },
    filename:'./layered_debug_keyboard.js'
    });
}

// Script to allow a user to add any keyboard to the keyboard menu
function addKeyboard(n)
{
  var sKbd;
  switch(n)
  {
    case 1:
      sKbd=document.getElementById('kbd_id1').value;
      keyman.addKeyboards(sKbd);
      break;
    case 2:
      sKbd=document.getElementById('kbd_id2').value.toLowerCase();
      keyman.addKeyboards('@'+sKbd);
      break;
    case 3:
      sKbd=document.getElementById('kbd_id3').value;
      keyman.addKeyboardsForLanguage(sKbd);
      break;
  }
}

function removeKeyboard(n)
{
    var sKbd=document.getElementById('kbd_id4').value;
    keyman.removeKeyboards(sKbd);
}

// Add keyboard on Enter (as well as pressing button)
function clickOnEnter(e,id)
{
  e = e || window.event;
  if(e.keyCode == 13) addKeyboard(id);
}

