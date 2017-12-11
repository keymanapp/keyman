/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/
// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the 
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) { 
  (function() {    
    // Declare KeymanWeb and util objects
    var keymanweb=window['keyman'], kbdInterface=window['KeymanWeb']=keymanweb['interface'], KeymanWeb=kbdInterface,
      util=keymanweb['util'], osk=keymanweb['osk'],device=util.device,dbg=keymanweb.debug; //osk defined here, build 350

    /**
     * Function     KSF
     * Scope        Public
     * Description  Save keyboard focus
     */    
    KeymanWeb['KSF'] = kbdInterface['saveFocus'] = kbdInterface.saveFocus = function() {
      keymanweb._IgnoreNextSelChange = 1;
    }
      
    /**
     * Function     KT
     * Scope        Public
     * @param       {string}      Ptext     Text to insert
     * @param       {?number}     PdeadKey  Dead key number, if any (???)
     * @return      {boolean}               true if inserted
     * Description  Insert text into active control
     */    
    KeymanWeb['KT'] = kbdInterface['insertText'] = kbdInterface.insertText = function(Ptext,PdeadKey) {
      kbdInterface.resetContextCache();
      //_DebugEnter('InsertText');
      var Lelem = keymanweb._LastActiveElement, Ls, Le, Lkc, Lsel, Lv=false;
      if(Lelem != null) {
        Ls=Lelem._KeymanWebSelectionStart;
        Le=Lelem._KeymanWebSelectionEnd;
        Lsel=keymanweb._Selection;

        keymanweb._IsActivatingKeymanWebUI = 1;
        keymanweb._IgnoreNextSelChange = 100;
        keymanweb._FocusLastActiveElement();
        if(keymanweb._IsMozillaEditableIframe(Lelem,0)) {
          Lelem = Lelem.documentElement;  // I3363 (Build 301)
        }
        if(document.selection  &&  Lsel != null) {
          Lsel.select();
        }
        Lelem._KeymanWebSelectionStart=Ls;
        Lelem._KeymanWebSelectionEnd=Le;
        keymanweb._IgnoreNextSelChange = 0;
        if(Ptext!=null) {
          kbdInterface.output(0, Lelem, Ptext);
        }
        if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
          kbdInterface.deadkeyOutput(0, Lelem, PdeadKey);
        }
        Lelem._KeymanWebSelectionStart=null;
        Lelem._KeymanWebSelectionEnd=null;
        Lv=true;
      }
      //_DebugExit('InsertText');
      return Lv;
    }
    
    /**
     * Function     registerKeyboard  KR                    
     * Scope        Public
     * @param       {Object}      Pk      Keyboard  object
     * Description  Register and load the keyboard
     */    
    KeymanWeb['KR'] = kbdInterface['registerKeyboard'] = kbdInterface.registerKeyboard = function(Pk) {
      // If initialization not yet complete, list the keyboard to be registered on completion of initialization
      if(!keymanweb['initialized']) {
        keymanweb.deferredKR.push(Pk); return;
      }
      
      var Li,Lstub;

      // Check if the active stub refers to this keyboard, else find applicable stub

      var Ps=keymanweb._ActiveStub, savedActiveStub = keymanweb._ActiveStub;
      if(!Ps || !('KI' in Ps) || (Ps['KI'] != Pk['KI'])) {         
        // Find the first stub for this keyboard
        for(Lstub=0;Lstub < keymanweb._KeyboardStubs.length; Lstub++) { // I1511 - array prototype extended
          Ps=keymanweb._KeyboardStubs[Lstub];
          if(Pk['KI'] == Ps['KI'])break;
          Ps=null;
        }
      } 
      
      // Build 369: ensure active stub defined when loading local keyboards 
      if(keymanweb._ActiveStub == null && Ps != null) {
        keymanweb._ActiveStub = Ps;
      }
      
      // Register the stub for this language (unless it is already registered)
      // keymanweb.KRS(Ps?Ps:Pk); 

      // Test if keyboard already loaded
      for(Li=0; Li<keymanweb._Keyboards.length; Li++) {
        if(Pk['KI'] == keymanweb._Keyboards[Li]['KI']) {
          return;
        }
      }
    
      // Append to keyboards array
      keymanweb._Keyboards=keymanweb._push(keymanweb._Keyboards,Pk);

      // Execute any external (UI) code needed after loading keyboard
      keymanweb.doKeyboardLoaded(Pk['KI']);

      // Restore the originally-active stub to its prior state.  No need to change it permanently.
      keymanweb._ActiveStub = savedActiveStub;
    }

    /**
     * Add the basic keyboard parameters (keyboard stub) to the array of keyboard stubs
     * If no language code is specified in a keyboard it cannot be registered, 
     * and a keyboard stub must be registered before the keyboard is loaded 
     * for the keyboard to be usable.
     * 
     * @param       {Object}      Pstub     Keyboard stub object
     * @return      {?number}               1 if already registered, else null
     */    
  //var ts0=new Date().toTimeString().substr(3,5);
    KeymanWeb['KRS'] = kbdInterface['registerStub'] = kbdInterface.registerStub = function(Pstub) {
      var Lk;
      
      // In initialization not complete, list the stub to be registered on completion of initialization
      if(!keymanweb['initialized']) {
        keymanweb.deferredKRS.push(Pstub);
        return null;
      }

      // The default stub is always the first keyboard stub loaded [and will be ignored by desktop browsers - not for beta, anyway]
      if(keymanweb.dfltStub == null) {
        keymanweb.dfltStub=Pstub;
        //if(device.formFactor == 'desktop') return 1;    //Needs further thought before release
      }

      // If no language code has been defined, and no stub has been registered for this keyboard, register with empty string as the language code
      if(typeof(Pstub['KP']) == 'undefined') {
        Pstub['KP'] = '';
      }
      if(typeof(Pstub['KLC']) == 'undefined') {
        Pstub['KLC'] = '';
      }
      if(typeof(Pstub['KL']) == 'undefined') {
        Pstub['KL'] = 'undefined';
      }

      // If language code already defined (or not specified in stub), check to see if stub already registered
      for(Lk=0; Lk<keymanweb._KeyboardStubs.length; Lk++) {
        if(keymanweb._KeyboardStubs[Lk]['KI'] == Pstub['KI']) {
          if(Pstub['KLC'] == '' || (keymanweb._KeyboardStubs[Lk]['KLC'] == Pstub['KLC'])) {
            return 1; // no need to register
          }
        }
      }
    
      // Register stub (add to KeyboardStubs array)
      keymanweb._KeyboardStubs=keymanweb._push(keymanweb._KeyboardStubs,Pstub);

      // TODO: Need to distinguish between initial loading of a large number of stubs and any subsequent loading.
      //   UI initialization should not be needed for each registration, only at end.
      // Reload this keyboard if it was the last active keyboard and 
      // make any changes needed by UI for new keyboard stub
      // (Uncommented for Build 360)
      keymanweb.doKeyboardRegistered(Pstub['KI'],Pstub['KL'],Pstub['KN'],Pstub['KLC'],Pstub['KP']);

      return null;
    }

    /**
     * Get *cached or uncached* keyboard context for a specified range, relative to caret
     * 
     * @param       {number}      n       Number of characters to move back from caret
     * @param       {number}      ln      Number of characters to return
     * @param       {Object}      Pelem   Element to work with (must be currently focused element)
     * @return      {string}              Context string 
     * 
     * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
     *             KC(2,1,Pelem) == "e"
     *             KC(3,3,Pelem) == "def"
     *             KC(10,10,Pelem) == "abcdef"  i.e. return as much as possible of the requested string
     */    
    
    KeymanWeb['KC'] = kbdInterface['context'] = kbdInterface.context = function(n, ln, Pelem) {
      var v = kbdInterface.cachedContext.get(n, ln);
      if(v !== null) {
        return v;
      }
      
      var r = keymanweb.KC_(n, ln, Pelem);
      kbdInterface.cachedContext.set(n, ln, r);
      return r;
    }
    
    /**
     * Function     nul           KN    
     * Scope        Public
     * @param       {number}      n       Length of context to check
     * @param       {Object}      Ptarg   Element to work with (must be currently focused element)
     * @return      {boolean}             True if length of context is less than or equal to n
     * Description  Test length of context, return true if the length of the context is less than or equal to n
     * 
     * Example     [abc|def] as INPUT, with the caret position marked by |:
     *             KN(3,Pelem) == TRUE
     *             KN(2,Pelem) == FALSE
     *             KN(4,Pelem) == TRUE
     */    
    KeymanWeb['KN'] = kbdInterface['nul'] = kbdInterface.nul = function(n, Ptarg) {
      var cx=kbdInterface.context(n+1, 1, Ptarg);
      
      // With #31, the result will be a replacement character if context is empty.
      return cx === "\uFFFE";
    }

    /**
     * Function     contextMatch  KCM   
     * Scope        Public
     * @param       {number}      n       Number of characters to move back from caret
     * @param       {Object}      Ptarg   Focused element
     * @param       {string}      val     String to match
     * @param       {number}      ln      Number of characters to return
     * @return      {boolean}             True if selected context matches val
     * Description  Test keyboard context for match
     */    
    KeymanWeb['KCM'] = kbdInterface['contextMatch'] = kbdInterface.contextMatch = function(n, Ptarg, val, ln) {
      //KeymanWeb._Debug('KeymanWeb.KCM(n='+n+', Ptarg, val='+val+', ln='+ln+'): return '+(kbdInterface.context(n,ln,Ptarg)==val)); 
      var cx=kbdInterface.context(n, ln, Ptarg);
      if(cx === val) {
        return true; // I3318
      }
      kbdInterface._DeadkeyResetMatched(); // I3318
      return false;
    }

    /**
     * Function     KIK      
     * Scope        Public
     * @param       {Object}  e   keystroke event
     * @return      {boolean}     true if keypress event
     * Description  Test if event as a keypress event
     */    
    KeymanWeb['KIK'] = kbdInterface['isKeypress'] = kbdInterface.isKeypress = function(e) {
      if(keymanweb._ActiveKeyboard['KM']) {   // I1380 - support KIK for positional layouts
        return !e.LisVirtualKey;             // will now return true for U_xxxx keys, but not for T_xxxx keys
      } else {
        return keymanweb._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
      }
    }
    
    /**
     * Function     keyMatch      KKM      
     * Scope        Public
     * @param       {Object}      e           keystroke event
     * @param       {number}      Lruleshift
     * @param       {number}      Lrulekey
     * @return      {boolean}                 True if key matches rule
     * Description  Test keystroke with modifiers against rule
     */    
    KeymanWeb['KKM'] = kbdInterface['keyMatch'] = kbdInterface.keyMatch = function(e,Lruleshift,Lrulekey) {
      var retVal = 0; // I3318
      var keyCode = (e.Lcode == 173 ? 189 : e.Lcode);  //I3555 (Firefox hyphen issue)

      var bitmask = keymanweb.getKeyboardModifierBitmask();

      if(e.vkCode > 255) {
        keyCode = e.vkCode; // added to support extended (touch-hold) keys for mnemonic layouts
      }
        
      if(e.LisVirtualKey || keyCode > 255) {
        if((Lruleshift & 0x4000) == 0x4000 || (keyCode > 255)) { // added keyCode test to support extended keys
          retVal = ((Lrulekey == keyCode) && ((Lruleshift & bitmask) == e.Lmodifiers)); //I3318, I3555
        }
      } else if((Lruleshift & 0x4000) == 0) {
        retVal = (keyCode == Lrulekey); // I3318, I3555
      }
      if(!retVal) {
        kbdInterface._DeadkeyResetMatched();  // I3318
      }
      return retVal != 0; // I3318
    };

    /**
     * Function     stateMatch    KSM
     * Scope        Public
     * @param       {Object}      e       keystroke event
     * @param       {number}      Lstate  
     * Description  Test keystroke against state key rules
     */
    KeymanWeb['KSM'] = kbdInterface['stateMatch'] = kbdInterface.stateMatch = function(e, Lstate) {
      return ((Lstate & e.Lstates) == Lstate);
    }

    /**
     * Function     keyInformation  KKI
     * Scope        Public
     * @param       {Object}      e
     * @return      {Object}              Object with event's virtual key flag, key code, and modifiers
     * Description  Get object with extended key event information
     */    
    KeymanWeb['KKI'] = kbdInterface['keyInformation'] = kbdInterface.keyInformation = function(e) {
      var ei = {};
      ei['vk'] = e.LisVirtualKey;
      ei['code'] = e.Lcode;
      ei['modifiers'] = e.Lmodifiers;
      return ei;
    };
    
    /**
     * Function     deadkeyMatch  KDM      
     * Scope        Public
     * @param       {number}      n       current cursor position
     * @param       {Object}      Ptarg   target element
     * @param       {number}      d       deadkey
     * @return      {boolean}             True if deadkey found selected context matches val
     * Description  Match deadkey at current cursor position
     */    
    KeymanWeb['KDM'] = kbdInterface['deadkeyMatch'] = kbdInterface.deadkeyMatch = function(n, Ptarg, d) {
      if(kbdInterface._DeadKeys.length == 0) {
        return false; // I3318
      }

      var sp=keymanweb._SelPos(Ptarg);
      n = sp - n;
      for(var i = 0; i < kbdInterface._DeadKeys.length; i++) {
        if(kbdInterface._DeadKeys[i].p == n  &&  kbdInterface._DeadKeys[i].d == d) {
          kbdInterface._DeadKeys[i].matched = 1;
          return true; // I3318
        }
      }
      kbdInterface._DeadkeyResetMatched(); // I3318

      return false;
    }
    
    /**
     * Function     beepReset   KBR      
     * Scope        Public
     * Description  Reset/terminate beep or flash (not currently used: Aug 2011)
     */    
    KeymanWeb['KBR'] = kbdInterface['beepReset'] = kbdInterface.beepReset = function() {
      kbdInterface.resetContextCache();
      
      var Lbo;
      kbdInterface._BeepTimeout = 0;
      for(Lbo=0;Lbo<kbdInterface._BeepObjects.length;Lbo++) { // I1511 - array prototype extended
        kbdInterface._BeepObjects[Lbo].e.style.backgroundColor = kbdInterface._BeepObjects[Lbo].c;
      }
      kbdInterface._BeepObjects = [];
    }
      
    /**
     * Function     beep          KB      
     * Scope        Public
     * @param       {Object}      Pelem     element to flash
     * Description  Flash body as substitute for audible beep
     */    
    KeymanWeb['KB'] = kbdInterface['beep'] = kbdInterface.beep = function(Pelem) {
      kbdInterface.resetContextCache();
      
      if(Pelem.body) {
        Pelem=Pelem.body; // I1446 - beep sometimes fails to flash when using OSK and rich control
      }
      
      if(!Pelem.style || typeof(Pelem.style.backgroundColor)=='undefined') {
        return;
      }

      for(var Lbo=0;Lbo<kbdInterface._BeepObjects.length;Lbo++) { // I1446 - beep sometimes fails to return background color to normal
                                                                  // I1511 - array prototype extended
        if(kbdInterface._BeepObjects[Lbo].e == Pelem) {
          return;
        }
      }
      
      kbdInterface._BeepObjects=keymanweb._push(kbdInterface._BeepObjects,{e:Pelem, c:Pelem.style.backgroundColor});
      Pelem.style.backgroundColor = '#000000';
      if(kbdInterface._BeepTimeout == 0) {
        kbdInterface._BeepTimeout = 1;
        window.setTimeout(kbdInterface.beepReset, 50);
      }
    }
    
    /**
     * Function     any           KA      
     * Scope        Public
     * @param       {number}      n     character position (index) 
     * @param       {string}      ch    character to find in string
     * @param       {string}      s     'any' string   
     * @return      {boolean}           True if character found in 'any' string, sets index accordingly
     * Description  Test for character matching
     */    
    KeymanWeb['KA'] = kbdInterface['any'] = kbdInterface.any = function(n,ch,s) {
      if(ch == '') {
        return false;
      }
      var Lix = s._kmwIndexOf(ch); //I3319
      kbdInterface._AnyIndices[n] = Lix;
      return Lix >= 0;
    }
    
    /**
     * Function     output        KO  
     * Scope        Public
     * @param       {number}      dn      number of characters to overwrite
     * @param       {Object}      Pelem   element to output to 
     * @param       {string}      s       string to output   
     * Description  Keyboard output
     */    
    KeymanWeb['KO'] = kbdInterface['output'] = kbdInterface.output = function(dn, Pelem, s) {
      kbdInterface.resetContextCache();
      
      // KeymanTouch for Android uses direct insertion of the character string
      if('oninserttext' in keymanweb) {
        keymanweb['oninserttext'](dn,s);
      }

      if(Pelem.body) {
        var Ldoc=Pelem;
      } else {
        var Ldoc=Pelem.ownerDocument;	// I1481 - integration with rich editors not working 100%
      }
      var Li, Ldv;
    
      if(Pelem.className.indexOf('keymanweb-input') >= 0) {
        var t=keymanweb.getTextBeforeCaret(Pelem);
        if(dn > 0) {
          t=t._kmwSubstr(0,t._kmwLength()-dn)+s; 
        } else {
          t=t+s;
        }
        keymanweb.setTextBeforeCaret(Pelem,t);
        return;
      }
    
      if (keymanweb.legacy) {
        if(dn>0) {
          Pelem.value=Pelem.value._kmwSubstr(0,Pelem.value._kmwLength()-dn)+s;  //I3319
        } else {
          Pelem.value=Pelem.value+s;
        }
      } else if (Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&  
          (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')      
        ) { // I2457 - support contentEditable elements in mozilla, webkit
        /* Editable iframe and contentEditable elements for mozilla */
        var _IsEditableIframe = Ldoc.designMode.toLowerCase() == 'on';
        if(_IsEditableIframe) {
          var _CacheableCommands = kbdInterface._CacheCommands(Ldoc);
        }
      
        var Lsel = Ldv.getSelection();
        var LselectionStart = Lsel.focusNode.nodeValue ? Lsel.focusNode.substringData(0,Lsel.focusOffset)._kmwLength() : 0;  // I3319
        
        if(!Lsel.isCollapsed) {
          Lsel.deleteFromDocument();  // I2134, I2192
        }
        //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

        var Lrange = Lsel.getRangeAt(0);
        if(dn > 0) {
          Lrange.setStart(Lsel.focusNode, Lsel.focusOffset - Lsel.focusNode.nodeValue.substr(0,Lsel.focusOffset)._kmwSubstr(-dn).length); // I3319
          Lrange.deleteContents();
        }

        //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

        if(s._kmwLength() > 0) { // I2132 - exception if s.length > 0, I3319
          if(Lsel.focusNode.nodeType == 3) {
            // I2134, I2192
            // Already in a text node
            //KeymanWeb._Debug('KO: Already in a text node, adding "'+s+'": '+Lsel.focusOffset + '-> '+Lsel.toString());
            var LfocusOffset = Lsel.focusOffset;
            //KeymanWeb._Debug('KO: node.text="'+Lsel.focusNode.data+'", node.length='+Lsel.focusNode.length);
            Lsel.focusNode.insertData(Lsel.focusOffset, s);
            try {
              Lsel.extend(Lsel.focusNode, LfocusOffset + s.length); 
            } catch(e) {
              // Chrome (through 4.0 at least) throws an exception because it has not synchronised its content with the selection.  scrollIntoView synchronises the content for selection
              Lsel.focusNode.parentNode.scrollIntoView();
              Lsel.extend(Lsel.focusNode, LfocusOffset + s.length);
            }
          } else {
            // Create a new text node - empty control
            //KeymanWeb._Debug('KO: Creating a new text node for "'+s+'"');
            var n = Ldoc.createTextNode(s);
            Lrange.insertNode(n);
            Lsel.extend(n,s.length);
          }
        }

        if(_IsEditableIframe) {
          kbdInterface._CacheCommandsReset(Ldoc, _CacheableCommands, null);// I2457 - support contentEditable elements in mozilla, webkit
        }
        
        Lsel.collapseToEnd();

        // Adjust deadkey positions 
        if(dn >= 0) {
          kbdInterface._DeadkeyDeleteMatched();                                  // I3318
          kbdInterface._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318
        } // Internet Explorer   (including IE9)   
      } else if(Ldoc  &&  (Ldv=Ldoc.selection)) { // build 77 - use elem.ownerDocument.selection
        if(Ldoc.body.isContentEditable || Ldoc.designMode.toLowerCase()=='on') { // I1295 - isContentEditable
          var _CacheableCommands = kbdInterface._CacheCommands(Ldoc);
        }

        var Lrange = Ldv.createRange(), Ls1;
        if(Lrange.text != '') {
          Ldv.clear();
          dn = 0;
        } else {
          Lrange.collapse(true);
        }

        if(dn > 0) {              
          Lrange.moveStart('character',-2*dn);  // I3319 (next four lines
          var s0=Lrange.text,s1=s0._kmwSubstr(-dn);
          Lrange.collapse(false); //move start back to end
          Lrange.moveStart('character',-s1.length);
        } else {
          dn = 0;
        }

        Lrange.text = s;

        if(Ldoc.body.isContentEditable || Ldoc.designMode.toLowerCase()=='on') { // I1295 - isContentEditable
          Lrange.moveStart('character',-s.length);
          
          kbdInterface._CacheCommandsReset(Ldoc, _CacheableCommands,Lrange.select);
          Lrange.moveStart('character',s.length);
          Lrange.select();
        }
        // Adjust deadkey positions 
        if(dn >= 0) {
          // Pelem.selectionStart seems to exist here in IE 9 and is valid.  This provides a possible approach, but may be wrong.
          // It appears safe to model the deadkey adjustment based on the non-IE9 code path's calculations.
          if(Pelem._KeymanWebSelectionStart != null) {// changed to allow a value of 0
            LselectionStart = Pelem._KeymanWebSelectionStart;
          } else {
            LselectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart);  // I3319
          }

          kbdInterface._DeadkeyDeleteMatched();                                  // I3318
          kbdInterface._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318
        }
  
        keymanweb._Selection = Ldv.createRange();
        keymanweb._Selection.select();
        keymanweb._Selection.scrollIntoView();
        // Mozilla et al; IE9+ also recognizes setSelectionRange, but does not seem to work in exactly the same way as Mozilla
      } else if (Pelem.setSelectionRange) {                                        
        var LselectionStart, LselectionEnd;
              
        if(Pelem._KeymanWebSelectionStart != null) {// changed to allow a value of 0
          LselectionStart = Pelem._KeymanWebSelectionStart;
          LselectionEnd = Pelem._KeymanWebSelectionEnd;
        } else {
          LselectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart);  // I3319
          LselectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);      // I3319
        }
        
        var LscrollTop, LscrollLeft;
        if(Pelem.type.toLowerCase() == 'textarea' && typeof(Pelem.scrollTop) != 'undefined') {
          LscrollTop = Pelem.scrollTop; LscrollLeft = Pelem.scrollLeft;
        }

        if(dn < 0) {// Don't delete, leave context alone (dn = -1)
          Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart) + s + Pelem.value._kmwSubstring(LselectionEnd);    //I3319
          dn = 0;
        } else if(LselectionStart < dn) {
          Pelem.value = s + Pelem.value._kmwSubstring(LselectionEnd); //I3319
        } else {
          Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart-dn) + s + Pelem.value._kmwSubstring(LselectionEnd); //I3319
        }

        // Adjust deadkey positions 
        if(dn >= 0) {
          kbdInterface._DeadkeyDeleteMatched(); // I3318
          kbdInterface._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318,I3319
        }

        if (typeof(LscrollTop) != 'undefined') {
          Pelem.scrollTop = LscrollTop;
          Pelem.scrollLeft = LscrollLeft;
        } 
        var caretPos=LselectionStart-dn+s._kmwLength();                   // I3319
        var caretPosUnits=Pelem.value._kmwCodePointToCodeUnit(caretPos);  // I3319
        
        Pelem.setSelectionRange(caretPosUnits,caretPosUnits);             // I3319
        Pelem._KeymanWebSelectionStart = null; Pelem._KeymanWebSelectionEnd = null;      
      }

      // Refresh element content after change (if needed)
      if(typeof(keymanweb.refreshElementContent) == 'function') {
        keymanweb.refreshElementContent(Pelem);
      }
    }
    
    /**
     * Function     deadkeyOutput KDO      
     * Scope        Public
     * @param       {number}      Pdn     no of character to overwrite (delete) 
     * @param       {Object}      Pelem   element to output to 
     * @param       {number}      Pd      deadkey id
     * Description  Record a deadkey at current cursor position, deleting Pdn characters first
     */    
    KeymanWeb['KDO'] = kbdInterface['deadkeyOutput'] = kbdInterface.deadkeyOutput = function(Pdn,Pelem,Pd) {
      kbdInterface.resetContextCache();
      var Lc = new Object();
      if(Pdn >= 0) {
        kbdInterface.output(Pdn,Pelem,"");  //I3318 corrected to >=
      }
      Lc.p=keymanweb._SelPos(Pelem); 
      Lc.d=Pd;
      kbdInterface._DeadKeys=keymanweb._push(kbdInterface._DeadKeys,Lc);
      
      //    _DebugDeadKeys(Pelem, 'KDeadKeyOutput: dn='+Pdn+'; deadKey='+Pd);
    }

    /**
     * Function     indexOutput   KIO      
     * Scope        Public
     * @param       {number}      Pdn     no of character to overwrite (delete) 
     * @param       {string}      Ps      string
     * @param       {number}      Pn      index
     * @param       {Object}      Pelem   element to output to 
     * Description  Output a character selected from the string according to the offset in the index array
     */    
    KeymanWeb['KIO'] = kbdInterface['indexOutput'] = kbdInterface.indexOutput = function(Pdn,Ps,Pn,Pelem) {
      kbdInterface.resetContextCache();
      if(kbdInterface._AnyIndices[Pn-1] < Ps._kmwLength()) {                        //I3319
        kbdInterface.output(Pdn,Pelem,Ps._kmwCharAt(kbdInterface._AnyIndices[Pn-1]));  //I3319
      }
    }

    /**
     * Function     _CacheCommands
     * Scope        Private
     * @param       {Object}    _Document
     * @return      {Array.<string>}        List of style commands that are cacheable
     * Description  Build reate list of styles that can be applied in iframes
     */    
    kbdInterface._CacheCommands = function(_Document) { // I1204 - style application in IFRAMEs, I2192, I2134, I2192   
      //var _CacheableBackColor=(_Document.selection?'hilitecolor':'backcolor');
      var _CacheableCommands=[
        ['backcolor',1],['fontname',1],['fontsize',1],['forecolor',1],['bold',0],
        ['italic',0],['strikethrough',0],['subscript',0],['superscript',0],['underline',0]
      ];
      if(_Document.defaultView) {
        keymanweb._push(_CacheableCommands,['hilitecolor',1]);
      }
        
      for(var n=0;n < _CacheableCommands.length; n++) { // I1511 - array prototype extended
        //KeymanWeb._Debug('Command:'+_CacheableCommands[n][0]);
        keymanweb._push(_CacheableCommands[n],_CacheableCommands[n][1] ?
            _Document.queryCommandValue(_CacheableCommands[n][0]) :
            _Document.queryCommandState(_CacheableCommands[n][0]));
      }
      return _CacheableCommands;
    }
    
    /**
     * Function     _CacheCommandReset
     * Scope        Private
     * @param       _Document
     *             _CacheableCommands
     *             _func      
     * @return      Nothing
     * Description  Restore styles in IFRAMEs (??)
     */    
    kbdInterface._CacheCommandsReset = function(_Document, _CacheableCommands, _func) {
      for(var n=0;n < _CacheableCommands.length; n++) { // I1511 - array prototype extended
        //KeymanWeb._Debug('ResetCacheCommand:'+_CacheableCommands[n][0]+'='+_CacheableCommands[n][2]);
        if(_CacheableCommands[n][1]) {
          if(_Document.queryCommandValue(_CacheableCommands[n][0]) != _CacheableCommands[n][2]) {
            if(_func) {
              _func();
            }
            _Document.execCommand(_CacheableCommands[n][0], false, _CacheableCommands[n][2]);
          }
        } else if(_Document.queryCommandState(_CacheableCommands[n][0]) != _CacheableCommands[n][2]) {
          if(_func) {
            _func();
          }
          //KeymanWeb._Debug('executing command '+_CacheableCommand[n][0]);
          _Document.execCommand(_CacheableCommands[n][0], false, null);
        }
      }
    }
    
    /**
     * KIFS compares the content of a system store with a string value 
     * 
     * @param       {number}      systemId    ID of the system store to test (only TSS_LAYER currently supported)
     * @param       {string}      strValue    String value to compare to
     * @param       {Object}      Pelem       Currently active element (may be needed by future tests)     
     * @return      {boolean}                 True if the test succeeds 
     */       
    KeymanWeb['KIFS'] = kbdInterface['ifStore'] = kbdInterface.ifStore = function(systemId,strValue,Pelem) {
      var result=true;
      if(systemId == kbdInterface.TSS_LAYER) {
        result = (osk.layerId === strValue);
      } else if(systemId == kbdInterface.TSS_PLATFORM) {
        var i,constraint,constraints=strValue.split(' ');
        for(i=0; i<constraints.length; i++) {
          constraint=constraints[i].toLowerCase();
          switch(constraint) {
            case 'touch':
            case 'hardware':
              if(util.activeDevice.touchable != (constraint == 'touch')) {
                result=false;
              }
          }

          switch(constraint) {
            case 'windows':
            case 'android':
            case 'ios':
            case 'macosx':
            case 'linux':
              if(util.activeDevice.OS.toLowerCase() != constraint) {
                result=false;
              }
          }

          switch(constraint) {
            case 'tablet':
            case 'phone':
            case 'desktop':
              if(util.activeDevice.formFactor != constraint) {
                result=false;
              }
          }

          switch(constraint) {
            case 'web':
              if(util.activeDevice.browser == 'native') {
                result=false; // web matches anything other than 'native'
              }
              break;
            case 'native':
            case 'ie':
            case 'chrome':
            case 'firefox':
            case 'safari':
            case 'opera':
              if(util.activeDevice.browser != constraint) {
                result=false;
              }
          }
        }
      }
      return result; //Moved from previous line, now supports layer selection, Build 350 
    }
  
    /**
     * KSETS sets the value of a system store to a string  
     * 
     * @param       {number}      systemId    ID of the system store to set (only TSS_LAYER currently supported)
     * @param       {string}      strValue    String to set as the system store content 
     * @param       {Object}      Pelem       Currently active element (may be needed in future tests)     
     * @return      {boolean}                 True if command succeeds
     *                                        (i.e. for TSS_LAYER, if the layer is successfully selected)
     */    
    KeymanWeb['KSETS'] = kbdInterface['setStore'] = function(systemId,strValue,Pelem) {
      kbdInterface.resetContextCache();
      if(systemId == kbdInterface.TSS_LAYER) {
        return osk.showLayer(strValue);     //Buld 350, osk reference now OK, so should work
      } else {
        return false;
      }
    }
  
    /**
     * Load an option store value from a cookie or default value
     * 
     * @param       {string}      kbdName     keyboard internal name
     * @param       {string}      storeName   store (option) name, embedded in cookie name
     * @param       {string}      dfltValue   default value
     * @return      {string}                  current or default option value   
     */    
    KeymanWeb['KLOAD'] = kbdInterface['loadStore'] = function(kbdName,storeName,dfltValue) {
      kbdInterface.resetContextCache();
      var cName='KeymanWeb_'+kbdName+'_Option_'+storeName,cValue=util.loadCookie(cName);
      if(typeof cValue[storeName] != 'undefined') {
        return unescape(cValue[storeName]);
      } else {
        return dfltValue;
      }
    }

    /**
     * Save an option store value to a cookie 
     * 
     * @param       {string}      storeName   store (option) name, embedded in cookie name
     * @param       {string}      optValue    option value to save
     * @return      {boolean}                 true if save successful
     */    
    KeymanWeb['KSAVE'] = kbdInterface['saveStore'] = function(storeName,optValue) {
      kbdInterface.resetContextCache();
      var kbd=keymanweb._ActiveKeyboard;
      if(!kbd || typeof kbd['KI'] == 'undefined' || kbd['KI'] == '') {
        return false;
      }
      
      var cName='KeymanWeb_'+kbd['KI']+'_Option_'+storeName, cValue=escape(optValue);

      util.saveCookie(cName,cValue);
      return true;
    }

  /**
    * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
    */
    KeymanWeb['GetLastActiveElement'] = function() {
      return keymanweb._LastActiveElement; 
    }

    KeymanWeb['FocusLastActiveElement'] = function() { 
      keymanweb._FocusLastActiveElement(); 
    }

    //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
    KeymanWeb['HideHelp'] = function() {
      osk._Hide(true);
    }

    KeymanWeb['ShowHelp'] = function(Px,Py) {
      osk._Show(Px,Py);
    }

    KeymanWeb['ShowPinnedHelp'] = function() {
      osk.userPositioned=true; osk._Show(-1,-1);
    }
    
    /**
     * Cache of context storing and retrieving return values from KC
     * Must be reset prior to each keystroke and after any text changes
     * MCD 3/1/14   
     **/         
    kbdInterface.cachedContext = {
      _cache: [],
      reset: function() { 
        this._cache = []; 
      },
      get: function(n, ln) { 
        // return null; // uncomment this line to disable context caching
        if(typeof this._cache[n] == 'undefined') {
          return null;
        } else if(typeof this._cache[n][ln] == 'undefined') {
          return null;
        }
        return this._cache[n][ln];
      },
      set: function(n, ln, val) { 
        if(typeof this._cache[n] == 'undefined') { 
          this._cache[n] = []; 
        } 
        this._cache[n][ln] = val; 
      }
    };

    kbdInterface.resetContextCache = function() {
      kbdInterface.cachedContext.reset();
    }
    
    // I3318 - deadkey changes START
    /**
     * Function     _DeadkeyResetMatched
     * Scope        Private
     * Description  Clear all matched deadkey flags
     */       
    kbdInterface._DeadkeyResetMatched = function() {                   
      var Li, _Dk = kbdInterface._DeadKeys;
      for(Li = 0; Li < _Dk.length; Li++) {
        _Dk[Li].matched = 0;
      }
    }

    /**
     * Function     _DeadkeyDeleteMatched
     * Scope        Private
     * Description  Delete matched deadkeys from context
     */       
    kbdInterface._DeadkeyDeleteMatched = function() {              
      var Li, _Dk = kbdInterface._DeadKeys;
      for(Li = 0; Li < _Dk.length; Li++) {
        if(_Dk[Li].matched) {
          _Dk.splice(Li,1);
        }
      }
    }

    /**
     * Function     _DeadkeyAdjustPos
     * Scope        Private
     * @param       {number}      Lstart      start position in context
     * @param       {number}      Ldelta      characters to adjust by   
     * Description  Adjust saved positions of deadkeys in context
     */       
    kbdInterface._DeadkeyAdjustPos = function(Lstart, Ldelta) {
      var Li, _Dk = kbdInterface._DeadKeys;
      for(Li = 0; Li < _Dk.length; Li++) {
        if(_Dk[Li].p > Lstart) {
          _Dk[Li].p += Ldelta;
        }
      }
    }

    kbdInterface.clearDeadkeys = function() {
      kbdInterface._DeadKeys = [];
    }
    // I3318 - deadkey changes END

    /**
     * Function     processKeystroke
     * Scope        Private
     * @param       {Object}        device      The device object properties to be utilized for this keystroke.
     * @param       {Object}        element     The page element receiving input
     * @param       {Object}        keystroke   The input keystroke (with its properties) to be mapped by the keyboard.
     * Description  Encapsulates calls to keyboard input processing.
     * @returns     {number}        0 if no match is made, otherwise 1.
     */
    kbdInterface.processKeystroke = function(device, element, keystroke) {
      // Clear internal state tracking data from prior keystrokes.
      keymanweb._CachedSelectionStart = null; // I3319     
      kbdInterface._DeadkeyResetMatched();       // I3318    
      kbdInterface.resetContextCache();

      // Ensure the settings are in place so that KIFS/ifState activates and deactivates
      // the appropriate rule(s) for the modeled device.
      util.activeDevice = device;

      // Calls the start-group of the active keyboard.
      return keymanweb._ActiveKeyboard['gs'](element, keystroke);
    }

  })();  
}
