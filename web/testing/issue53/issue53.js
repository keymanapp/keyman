// JavaScript Document samplehdr.js: Keyboard management for KeymanWeb demonstration pages

/* 
    The keyboard name and/or BCP-47 code must be specified for each keyboard that is to be available.
    If the same keyboard is used for several languages, it must be listed for each
    language, but the keyboard itself will only be loaded once. 
    If two (or more) keyboards are to be available for a given language, both must be listed.
    Any number of keyboards may be specified in one or more calls. 
    Keyboard paths may be absolute (with respect to the server root) or relative to the keyboards option path. 
    The actual keyboard object will be downloaded asynchronously when first selected for use.
  
    Each argument to addKeyboards() is a string, for example:
      european2         loads the current version of the Eurolatin 2 keyboard (for its default language)
      european2@fr     loads the current version of the Eurolatin 2 keyboard for French
      european2@fr@1.2 loads version 1.2 of the Eurolatin 2 keyboard for French
      
    Argument syntax also supports the following extensions:
      @fr              load the current version of the default keyboard for French
      @fr$             load all available keyboards (current version) for French
          
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

  function loadKeyboards() 
  { 
    var kmw=keyman;

    // Add a fully-specified, locally-sourced, keyboard.
    kmw.addKeyboards({id:'layered_debug_keyboard',name:'Web_Layer_Debugging',
      languages:{
        id:'dbg',name:'Debug',region:'North America'
        },
      filename:'./layered_debug_keyboard-1.0.js'
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

