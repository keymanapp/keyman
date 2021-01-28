function loadKeyboards() {
  var commonPrefix = "/resources";

  if(location.protocol == "file:") {
    // Note:  won't actually work, as it'll be blocked by CORS / same-origin policy when
    // KeymanWeb tries to load the stub via the file: protocol.
    //
    // Both Firefox and Chrome don't like that it's not data: or http:/https:
    // and will consider the local files to be "remote resource[s]".
    commonPrefix = "../../../common/core/web/tests/resources";
  }

  var filePrefix = commonPrefix + "/json/keyboards/";

  // Existing unit_test stubs at the time of writing:
  var preloadFiles = [
    filePrefix + "galaxie_hebrew_positional.json",
    filePrefix + "khmer_angkor.json",
    filePrefix + "lao_2008_basic.json",
    filePrefix + "options_with_save.json",
    filePrefix + "test_deadkeys.json",
    filePrefix + "test_simple_deadkeys.json",
    filePrefix + "test_chirality.json"
  ];

  loadExistingStubs(preloadFiles);
}

// Script to allow a user to add any keyboard to the keyboard menu 
function addKeyboard(n) { 
  var sKbd;
  switch(n) {
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
    case 4:
      sKbd=document.getElementById('kbd_stub_add').value;
      var stub = new KMWRecorder.KeyboardStub(JSON.parse(sKbd));
      stub.setBasePath(commonPrefix + "/keyboards", false);
      keyman.addKeyboards(stub);

      // We actually know the exact details for this one easily, so set it as active!
      doKeyboardChange("Keyboard_" + stub.id, stub.getFirstLanguage());
      break;
  }
}

// Add keyboard on Enter (as well as pressing button)
function clickOnEnter(e,id) {                                       
  e = e || window.event;
  if(e.keyCode == 13) addKeyboard(id); 
}

