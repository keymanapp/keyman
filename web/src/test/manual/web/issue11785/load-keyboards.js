// JavaScript Document samplehdr.js: Keyboard management for KeymanWeb demonstration pages

/*
    The keyboard name and/or BCP-47 language code must be specified for each keyboard that is to be available.
    If the same keyboard is used for several languages, it must be listed for each
    language, but the keyboard itself will only be loaded once.
    If two (or more) keyboards are to be available for a given language, both must be listed.
    Any number of keyboards may be specified in one or more calls.
    Keyboard paths may be absolute (with respect to the server root) or relative to the keyboards option path.
    The actual keyboard object will be downloaded asynchronously when first selected for use.

    Each argument to addKeyboards() is a string, for example:
      european2         loads the current version of the Eurolatin 2 keyboard (for its default language)
      european2@fr      loads the current version of the Eurolatin 2 keyboard for French
      european2@fr@1.2  loads version 1.2 of the Eurolatin 2 keyboard for French

    Argument syntax also supports the following extensions:
      @fr               load the current version of the default keyboard for French
      @fr$              load all available keyboards (current version) for French

    Each call to addKeyboards() requires a single call to the remote server,
    (unless all keyboards listed are local and fully specified) so it is better
    to use multiple arguments rather than separate function calls.

    Calling addKeyboards() with no arguments returns a list of *all* available keyboards.
    The Toolbar (desktop browser) UI is best suited for allowing users to select
    the appropriate language and keyboard in this case.

    Keyboards may also be specified by language name using addKeyboardsForLanguage()
    for example:
      keymanweb.addKeyboardsForLanguage('Burmese');

    Appending $ to the language name will again cause all available keyboards for that
    language to be loaded rather than the default keyboard.

    The first call to addKeyboardsForLanguage() makes an additional call to the
    keyman API to load the current list of keyboard/language associations.

    In this example, the following function loads the indicated keyboards,
    and is called when the page loads.
*/

  function errToString(err) {
    // Painful?  Kinda.  But needed on un-updated Android API 21!
    if(Array.isArray(err)) {
      var result = '';
      for(var i = 0; i < err.length; i++) {
        var e = err[i];
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

  function loadKeyboards(nestLevel)
  {
    var kmw=keyman;

    var base_prefix = '../';
    var prefix = './'; // The default - when prefix == 0.

    if(nestLevel !== undefined && nestLevel > 0) {
      prefix = '';
      for(var i=0; i < nestLevel; i++) {
        prefix = prefix + base_prefix;
      }
    }

    // The first keyboard added will be the default keyboard for touch devices.
    // For faster loading, it may be best for the default keyboard to be
    // locally sourced.
    // doAddKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    //   filename:(prefix + 'us-1.0.js')});

    // Do NOT link the us-1.0 keyboard here!

    doAddKeyboards({id:'test_chirality',name:'test_chirality',languages:{id:'en',name:'English'},
      filename:(prefix + 'test_chirality.js')});

    doAddKeyboards({id:'obolo_chwerty_6351',name:'obolo_chwerty_6351',languages:{id:'en',name:'English'},
      filename:(prefix + 'obolo_chwerty_6351.js')});

    doAddKeyboards({id:'gesture_prototyping',name:'Gesture Prototyping',languages:{id:'en',name:'English'},
      filename:(prefix + 'keyboards/gesture_prototyping/build/gesture_prototyping.js')});

    doAddKeyboards({id:'diacritic_rota',name:'Diacritic 10-key Rota',languages:{id:'en',name:'English'},
      filename:(prefix + 'keyboards/diacritic_rota/build/diacritic_rota.js')});

    doAddKeyboards({id:'ye_old_ten_key',name:'Classic 10-key',languages:{id:'en',name:'English'},
      filename:(prefix + 'keyboards/ye_old_ten_key/build/ye_old_ten_key.js')});

      // Add more keyboards to the language menu, by keyboard name,
    // keyboard name and language code, or just the BCP-47 language code.
    // We use a different loading pattern here than in the samples version to provide a slightly different set of test cases.
    doAddKeyboards('french','@he');
    doAddKeyboards('khmer_angkor','@km');
    doAddKeyboards({id:'sil_euro_latin', name:'SIL EuroLatin', languages: [{id:'no'}, {id:'sv'}]}); // Loads from partial stub instead of the compact string.

    // Add a keyboard by language name.  Note that the name must be spelled
    // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
    // usually easier.)
    doAddKeyboardsForLanguage('Dzongkha');

    // Add a fully-specified, locally-sourced, keyboard with custom font
    doAddKeyboards({id:'lao_2008_basic',name:'Lao Basic',
      languages: {
          id:'lo',name:'Lao',region:'Asia',
        },
      filename:(prefix + 'lao_2008_basic-1.2.js')
      });

    // The following two optional calls should be delayed until language menus are fully loaded:
    //  (a) a specific mapped input element input is focused, to ensure that the OSK appears
    //  (b) a specific keyboard is loaded, rather than the keyboard last used.
    //window.setTimeout(function(){kmw.setActiveElement('ta1',true);},2500);
    //window.setTimeout(function(){kmw.setActiveKeyboard('Keyboard_french','fr');},3000);

    // Note that locally specified keyboards will be listed before keyboards
    // requested from the remote server by user interfaces that do not order
    // keyboards alphabetically by language.
  }

  // Script to allow a user to add any keyboard to the keyboard menu
  function addKeyboard(n)
  {
    var sKbd,kmw=keyman;
    switch(n)
    {
      case 1:
        sKbd=document.getElementById('kbd_id1').value;
        doAddKeyboards(sKbd);
        break;
      case 2:
        sKbd=document.getElementById('kbd_id2').value.toLowerCase();
        doAddKeyboards('@'+sKbd);
        break;
      case 3:
        // Add keyboard for comma-separated language name(s)
        sKbd=document.getElementById('kbd_id3').value;
        doAddKeyboardsForLanguage(sKbd);
        break;
    }
  }

  // Add keyboard on Enter (as well as pressing button)
  function clickOnEnter(e,id)
  {
    e = e || window.event;
    if(e.keyCode == 13) addKeyboard(id);
  }

