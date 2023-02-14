// JavaScript Document promisehdr.js: Keyboard management for KeymanWeb demonstration pages

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

/**
 * Add keyboards
 * @param {string | KeyboardStub | (string|KeyboardStub)[]} obj Info for adding a keyboard
 */
  function addKeyboards(...args) {
    var kmw=keyman;
    var promise = kmw.addKeyboards(args);
    promise.then(result => {
      if (result.length > 0 && !result[0].error) {
        console.log('Adding: ', result);
      } else {
        console.log('Nothing added');
      }
    }).catch(errorStubs => {
      // Consumer decides how to handle errors
      errorStubs.forEach(e => console.error(e));

      // We'll also concat the error messages to an alert
      var errorMessages = errorStubs.map(e => e.error.message).join('\n');
      alert(errorMessages);
    });
  }

  /**
   * Add a keyboard by language name.  Note that the name must be spelled
     correctly, or the keyboard will not be found.  (Using BCP-47 codes is
     usually easier.)
   * @param {string} languages. Could be comma-separated languages
   */
  function addKeyboardsForLanguage(languages) {
    var kmw=keyman;
    kmw.addKeyboardsForLanguage(languages.split(',').map(item => item.trim())).then(result => {
      console.log('Adding ' + languages + ': ', result);
    }).catch(errorStubs => {
      // Consumer decides how to handle errors
      errorStubs.forEach(e => console.error(e));

      // We'll also concat the error messages to an alert
      var errorMessages = errorStubs.map(e => e.error.message).join('\n');
      alert(errorMessages);
    });
  }

  function loadKeyboards()
  {
    // No parameter adds entire cloud keyboard catalog
    //addKeyboards();

    // The first keyboard added will be the default keyboard for touch devices.
    // For faster loading, it may be best for the default keyboard to be
    // locally sourced.
    addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
      filename:'../us-1.0.js'});

    // Add more keyboards to the language menu, by keyboard name,
    // keyboard name and language code, or just the BCP-47 language code.
    // We use a different loading pattern here than in the samples version to provide a slightly different set of test cases.
    addKeyboards('spanish','@nqo');
    // One keyboard - 2 languages
    addKeyboards({id:'sil_euro_latin', name:'SIL EuroLatin', languages: [{id:'no'}, {id:'sv'}]}); // Loads from partial stub instead of the compact string.

    // One invalid keyboard
    addKeyboards({id: 'invalid', name:'Invalid'});

    // One keyboard, 1 invalid
    addKeyboards({id:'sil_cameroon_azerty', name:'SIL Cameroon Azerty', languages:[{id:'aal-latn'}]},
                 {id: 'invalid2', name:'Invalid2'});

    // Add a keyboard by language name.  Note that the name must be spelled
    // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
    // usually easier.)
    addKeyboardsForLanguage('Dzongkha');

    // Add a fully-specified, locally-sourced, keyboard with custom font
    addKeyboards({id:'sil_cameroon_qwerty',name:'Cameroon QWERTY (SIL)',
      languages:{
        id:'aal-Latn',name:'Afade (Latin)',region:'Africa',
        font:{family:'AndikaAfr',source:['./ANDIKAAFR-R.TTF']}
        },
      filename:'./sil_cameroon_qwerty.js'
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
        addKeyboards(sKbd);
        break;
      case 2:
        sKbd=document.getElementById('kbd_id2').value.toLowerCase();
        kmw.addKeyboards('@'+sKbd);
        break;
      case 3:
        // Add keyboard for comma-separated language name(s)
        sKbd=document.getElementById('kbd_id3').value;
        addKeyboardsForLanguage(sKbd);
        break;
      case 4:
        // Add khmer_angkor keyboard with languages Object and no filename
        addKeyboards({id:'khmer_angkor', name: 'Khmer Angkor', languages:{id:'km'}});
        break;
    }
  }

  // Add keyboard on Enter (as well as pressing button)
  function clickOnEnter(e,id)
  {
    e = e || window.event;
    if(e.keyCode == 13) addKeyboard(id);
  }

