// Modified version of commonHeader - contains repeated requests for the same language.

  async function loadKeyboards() {
    // The first keyboard added will be the default keyboard for touch devices.
    // For faster loading, it may be best for the default keyboard to be
    // locally sourced.
    await keyman.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
      filename:'../us-1.0.js'});

    // Add more keyboards to the language menu, by keyboard name,
    // keyboard name and language code, or just the BCP-47 language code.
    await keyman.addKeyboards('french','european2@sv','european2@sv','european2@sv','european2@sv','european2@sv','european2@sv','european2@no','@he');
    await keyman.addKeyboards('@he');
    await keyman.addKeyboards('@he');

    // Add a keyboard by language name.  Note that the name must be spelled
    // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
    // usually easier.)
    await keyman.addKeyboardsForLanguage('Dzongkha');

    // Add a fully-specified, locally-sourced, keyboard with custom font
    await keyman.addKeyboards({id:'lao_2008_basic',name:'Lao Basic',
      languages:{
        id:'lo',name:'Lao',region:'Asia'
        },
      filename:'../lao_2008_basic.js'
      });

    // The following two optional calls should be delayed until language menus are fully loaded:
    //  (a) a specific mapped input element input is focused, to ensure that the OSK appears
    //  (b) a specific keyboard is loaded, rather than the keyboard last used.
    //window.setTimeout(function(){keyman.setActiveElement('ta1',true);},2500);
    //window.setTimeout(function(){keyman.setActiveKeyboard('Keyboard_french','fr');},3000);

    // Note that locally specified keyboards will be listed before keyboards
    // requested from the remote server by user interfaces that do not order
    // keyboards alphabetically by language.
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

