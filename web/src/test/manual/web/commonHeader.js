/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

function errToString(err) {
  // Painful?  Kinda.  But needed on un-updated Android API 21!
  if(Array.isArray(err)) {
    let result = '';
    for(let i = 0; i < err.length; i++) {
      const e = err[i];
      if(e.error instanceof Error) {
        result += e.error.message + '\n';
      } else {
        result += JSON.stringify(e) + '\n';
      }
    }
    return result;
  }
  if(err instanceof Error) {
    return err.message;
  }
  return JSON.stringify(err);
}

function doAddKeyboards(data) {
  return keyman.addKeyboards(data).catch(function(err) {
    console.error('keyman.addKeyboards failed with '+errToString(err)+' for '+JSON.stringify(data));
  });
}

function doAddKeyboardsForLanguage(data) {
  return keyman.addKeyboardsForLanguage(data).catch(function(err) {
    console.error('keyman.addKeyboardsForLanguage failed with '+errToString(err)+' for '+JSON.stringify(data));
  });
}

async function loadKeyboards(nestLevel) {
  const base_prefix = '../';
  let prefix = './'; // The default - when prefix == 0.

  if(nestLevel !== undefined && nestLevel > 0) {
    prefix = '';
    for(let i=0; i < nestLevel; i++) {
      prefix += base_prefix;
    }
  }

  // The first keyboard added will be the default keyboard for touch devices.
  // For faster loading, it may be best for the default keyboard to be
  // locally sourced.
  await doAddKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    filename:(prefix + 'us-1.0.js')});

  await doAddKeyboards({id:'web_context_tests',name:'Web Context Tests',languages:{id:'en',name:'English'},
    filename:(prefix + 'web_context_tests.js')});

  await doAddKeyboards({id:'test_chirality',name:'test_chirality',languages:{id:'en',name:'English'},
    filename:(prefix + 'test_chirality.js')});

  await doAddKeyboards({id:'obolo_chwerty_6351',name:'obolo_chwerty_6351',languages:{id:'en',name:'English'},
    filename:(prefix + 'obolo_chwerty_6351.js')});

  await doAddKeyboards({id:'gesture_prototyping',name:'Gesture Prototyping',languages:{id:'en',name:'English'},
    filename:(prefix + 'keyboards/gesture_prototyping/build/gesture_prototyping.js')});

  await doAddKeyboards({id:'diacritic_rota',name:'Diacritic 10-key Rota',languages:{id:'en',name:'English'},
    filename:(prefix + 'keyboards/diacritic_rota/build/diacritic_rota.js')});

  await doAddKeyboards({id:'ye_old_ten_key',name:'Classic 10-key',languages:{id:'en',name:'English'},
    filename:(prefix + 'keyboards/ye_old_ten_key/build/ye_old_ten_key.js')});

  // Add more keyboards to the language menu, by keyboard name,
  // keyboard name and language code, or just the BCP-47 language code.
  // We use a different loading pattern here than in the samples version to provide a slightly different set of test cases.
  await doAddKeyboards('french','@he');
  await doAddKeyboards('khmer_angkor','@km');
  await doAddKeyboards({id:'sil_euro_latin', name:'SIL EuroLatin', languages: [{id:'no'}, {id:'sv'}]}); // Loads from partial stub instead of the compact string.

  // Add a keyboard by language name.  Note that the name must be spelled
  // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
  // usually easier.)
  await doAddKeyboardsForLanguage('Dzongkha');

  // Add a fully-specified, locally-sourced, keyboard with custom font
  await doAddKeyboards({
    id: 'lao_2008_basic',
    name: 'Lao Basic',
    languages: {
        id: 'lo', name: 'Lao', region: 'Asia'
    },
    filename: (prefix + 'lao_2008_basic-1.2.js')
  });
}

// Script to allow a user to add any keyboard to the keyboard menu
async function addKeyboard(n) {
  let sKbd;
  switch(n) {
    case 1:
      sKbd=document.getElementById('kbd_id1').value;
      await doAddKeyboards(sKbd);
      break;
    case 2:
      sKbd=document.getElementById('kbd_id2').value.toLowerCase();
      await doAddKeyboards('@'+sKbd);
      break;
    case 3:
      // Add keyboard for comma-separated language name(s)
      sKbd=document.getElementById('kbd_id3').value;
      await doAddKeyboardsForLanguage(sKbd);
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

