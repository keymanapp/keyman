/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Modified version of commonHeader.js - contains repeated requests for the same language.
 */

async function loadKeyboards() {
  await keyman.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    filename:'../us-1.0.js'});

  // Intentionally request the same keyboard multiple times to test that it
  // is not added multiple times and doesn't cause an error.
  await keyman.addKeyboards('french', 'european2@sv', 'european2@sv', 'european2@sv',
    'european2@sv', 'european2@sv', 'european2@sv', 'european2@no', '@he');
  await keyman.addKeyboards('@he');
  await keyman.addKeyboards('@he'); // intentional duplicate

  // Add a keyboard by language name.  Note that the name must be spelled
  // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
  // usually easier.)
  await keyman.addKeyboardsForLanguage('Dzongkha');

  // Add a fully-specified, locally-sourced, keyboard with custom font
  await keyman.addKeyboards({
    id: 'lao_2008_basic',
    name: 'Lao Basic',
    languages:{
      id: 'lo',
      name: 'Lao',
      region: 'Asia'
    },
    filename: '../lao_2008_basic.js'
  });
}

// Script to allow a user to add any keyboard to the keyboard menu
async function addKeyboard(n)
{
  let sKbd;
  switch(n)
  {
    case 1:
      sKbd=document.getElementById('kbd_id1').value;
      await keyman.addKeyboards(sKbd);
      break;
    case 2:
      sKbd=document.getElementById('kbd_id2').value.toLowerCase();
      await keyman.addKeyboards('@'+sKbd);
      break;
    case 3:
      sKbd=document.getElementById('kbd_id3').value;
      await keyman.addKeyboardsForLanguage(sKbd);
      break;
  }
}

// Add keyboard on Enter (as well as pressing button)
async function clickOnEnter(e,id) {
  e = e || window.event;
  if (e.keyCode == 13) {
    await addKeyboard(id);
  }
}

