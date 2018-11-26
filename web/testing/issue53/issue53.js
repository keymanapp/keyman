function loadKeyboards()
{
  var kmw=keyman;

  // Add a fully-specified, locally-sourced, keyboard.
  kmw.addKeyboards({id:'layered_debug_keyboard',name:'Web_Layer_Debugging',
    languages:{
      id:'dbg',name:'Debug',region:'North America'
      },
    filename:'./layered_debug_keyboard.js'
    });
}

// Script to allow a user to add any keyboard to the keyboard menu
function addKeyboard(n)
{
  var sKbd,kmw=keyman;
  switch(n)
  {
    case 1:
      sKbd=document.getElementById('kbd_id1').value;
      kmw.addKeyboards(sKbd);
      break;
    case 2:
      sKbd=document.getElementById('kbd_id2').value.toLowerCase();
      kmw.addKeyboards('@'+sKbd);
      break;
    case 3:
      sKbd=document.getElementById('kbd_id3').value;
      kmw.addKeyboardsForLanguage(sKbd);
      break;
  }
}

function removeKeyboard(n)
{
    var sKbd=document.getElementById('kbd_id4').value, kmw=keyman;
    kmw["removeKeyboards"](sKbd);
}

// Add keyboard on Enter (as well as pressing button)
function clickOnEnter(e,id)
{
  e = e || window.event;
  if(e.keyCode == 13) addKeyboard(id);
}

