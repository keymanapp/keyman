/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
async function loadKeyboards()
{
  await keyman.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    filename:'../us-1.0.js'});
  await keyman.addKeyboards('french','european2@sv','european2@no','@he');
  await keyman.addKeyboardsForLanguage('Dzongkha');
  await keyman.addKeyboards({id:'lao_2008_basic',name:'Lao Basic',
    languages:{
      id:'lo',name:'Lao',region:'Asia',
      font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
      },
    filename:'../lao_2008_basic-1.2.js'
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
async function clickOnEnter(e,id)
{
  e = e || window.event;
  if (e.keyCode == 13) {
    await addKeyboard(id);
  }
}

async function removeKeyboard() {
  let sKbd = document.getElementById('kbd_id4').value;
  let result = await keyman.removeKeyboards(sKbd);

  console.log("Keyboard '" + sKbd + "' removal success: " + result);
}

// Removes keyboard on Enter (as well as pressing button)
async function removeOnEnter(e)
{
  e = e || window.event;
  if(e.keyCode == 13) {
    await removeKeyboard();
  }
}
