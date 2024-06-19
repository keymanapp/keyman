// Modified version of commonHeader - contains repeated requests for the same language.

  function loadKeyboards() 
  { 
    var kmw=keyman;
    
    // The first keyboard added will be the default keyboard for touch devices.
    // For faster loading, it may be best for the default keyboard to be
    // locally sourced.
    kmw.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
      filename:'../us-1.0.js'});
      
    // Add more keyboards to the language menu, by keyboard name,
    // keyboard name and language code, or just the BCP-47 language code.
    kmw.addKeyboards('french','european2@sv','european2@sv','european2@sv','european2@sv','european2@sv','european2@sv','european2@no','@he');
    kmw.addKeyboards('@he');
    kmw.addKeyboards('@he');
  
    // Add a keyboard by language name.  Note that the name must be spelled
    // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
    // usually easier.)
    kmw.addKeyboardsForLanguage('Dzongkha');
    
    // Add a fully-specified, locally-sourced, keyboard with custom font  
    kmw.addKeyboards({id:'lao_2008_basic',name:'Lao Basic',
      languages:{
        id:'lo',name:'Lao',region:'Asia'
        },
      filename:'../lao_2008_basic.js'
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
  
  // Add keyboard on Enter (as well as pressing button)
  function clickOnEnter(e,id)
  {                                       
    e = e || window.event;
    if(e.keyCode == 13) addKeyboard(id); 
  }

