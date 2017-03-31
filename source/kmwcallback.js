/***
   KeymanWeb 2.0
   Copyright 2014 Tavultesoft Pty Ltd

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***/

(function() 
{
  // Declare KeymanWeb and util objects
  var keymanweb=window['tavultesoft']['keymanweb'], util=keymanweb['util'], 
    osk=keymanweb['osk'],device=util.device,dbg=keymanweb.debug;     //osk defined here, build 350
  
  keymanweb.TSS_LAYER = 33;
  keymanweb.TSS_PLATFORM = 31;
  
  keymanweb._BeepObjects=[];
  keymanweb._BeepTimeout=0;

  /**
   * Function     KSF
   * Scope        Public
   * Description  Save keyboard focus
   */    
  keymanweb['KSF'] = keymanweb.KSF = function()     // KeyboardSaveFocus
  {
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
  keymanweb['KT'] = keymanweb.KT = function(Ptext,PdeadKey)  // KeyboardInsertText
  {
    keymanweb.cachedContext.reset();
    //_DebugEnter('InsertText');  
    var Lelem = keymanweb._LastActiveElement, Ls, Le, Lkc, Lsel, Lv=false;
    if(Lelem != null)
    {
      Ls=Lelem._KeymanWebSelectionStart;
      Le=Lelem._KeymanWebSelectionEnd;
      Lsel=keymanweb._Selection;
      //if(!Lsel || Lsel.parentElement() == Lelem)  // I1506
      //{
        keymanweb._IsActivatingKeymanWebUI = 1;
        keymanweb._IgnoreNextSelChange = 100;
        keymanweb._FocusLastActiveElement();
        if(keymanweb._IsMozillaEditableIframe(Lelem,0)) Lelem = Lelem.documentElement;  // I3363 (Build 301)
        if(document.selection  &&  Lsel != null) Lsel.select();
        Lelem._KeymanWebSelectionStart=Ls;
        Lelem._KeymanWebSelectionEnd=Le;
        keymanweb._IgnoreNextSelChange = 0;
        if(Ptext!=null)keymanweb.KO(0, Lelem, Ptext);
        if(typeof(PdeadKey)!='undefined') keymanweb.KDO(0, Lelem, PdeadKey);
        Lelem._KeymanWebSelectionStart=null;
        Lelem._KeymanWebSelectionEnd=null;
        Lv=true;
      //}
    }
    //_DebugExit('InsertText');
    return Lv;
  }
  
  /**
   * Function     KR                    
   * Scope        Public
   * @param       {Object}      Pk      Keyboard  object
   * Description  Register and load the keyboard
   */    
  keymanweb['KR'] = keymanweb.KR = function(Pk)
  {
    // Clear the load failure timer, but only if this is the keyboard
    // that is currently pending (KMEW-101)
    if(keymanweb.loadTimer && (Pk['KI'] == keymanweb._LoadingInternalName))
    {
        window.clearTimeout(keymanweb.loadTimer); keymanweb.loadTimer=null;
    }

    // If initialization not yet complete, list the keyboard to be registered on completion of initialization
    if(!keymanweb['initialized'])
    {          
      keymanweb.deferredKR.push(Pk); return;
    }
    
    var Li,Lstub;

    // Check if the active stub refers to this keyboard, else find applicable stub
    var Ps=keymanweb._ActiveStub;     
    if(!Ps || !('KI' in Ps) || (Ps['KI'] != Pk['KI']))
    {             
      // Find the first stub for this keyboard
      for(Lstub=0;Lstub < keymanweb._KeyboardStubs.length; Lstub++)  // I1511 - array prototype extended
      {  
        Ps=keymanweb._KeyboardStubs[Lstub];
        if(Pk['KI'] == Ps['KI'])break;        
        Ps=null;
      }
    } 
     
    // Build 369: ensure active stub defined when loading local keyboards 
    if(keymanweb._ActiveStub == null && Ps != null) keymanweb._ActiveStub = Ps;  
    
    // The following should never be needed or even appropriate - the keyboard object does not
    // override stub information, but simply adds to it.  The stub contains meta-data which either
    // overriddes or extends the keyboard object data.  (Sept 2014)
    
    // Register the stub for this language (unless it is already registered)
    // keymanweb.KRS(Ps?Ps:Pk); 

    // Test if keyboard already loaded
    for(Li=0; Li<keymanweb._Keyboards.length; Li++)
    {
      if(Pk['KI'] == keymanweb._Keyboards[Li]['KI']) return;
    }
   
    // Append to keyboards array
    keymanweb._Keyboards=keymanweb._push(keymanweb._Keyboards,Pk);

    if(keymanweb._LoadingInternalName == Pk['KI']) 
    {   
      keymanweb.doBeforeKeyboardChange(Pk['KI'],Ps['KLC']);
      keymanweb._ActiveKeyboard=Pk;

      if(keymanweb._LastActiveElement != null) 
      {
        keymanweb._JustActivatedKeymanWebUI = 1;
        keymanweb._SetTargDir(keymanweb._LastActiveElement);            
      }
      keymanweb._LoadingInternalName = null;
      
      // Copy remaining properties from stub --TODO: this is not right, the keyboard does NOT have these properties
      Pk['KL'] = typeof Ps['KL'] != 'undefined' ? Ps['KL'] : '';  // I1300 - Language support, I2309 - errors loading keyboards without all K?? properties defined
      Pk['KLC'] = typeof Ps['KLC'] != 'undefined' ? Ps['KLC'] : '';
      Pk['KR'] = typeof Ps['KR'] != 'undefined' ? Ps['KR'] : '';
      Pk['KRC'] = typeof Ps['KRC'] != 'undefined' ? Ps['KRC'] : '';
      Pk['KC'] = typeof Ps['KC'] != 'undefined' ? Ps['KC'] : '';
      Pk['KCC'] = typeof Ps['KCC'] != 'undefined' ? Ps['KCC'] : '';
      Pk['KD'] = typeof Ps['KD'] != 'undefined' ? Ps['KD'] : '';  
      Pk['KS'] = typeof Ps['KS'] != 'undefined' ? Ps['KS'] : 0; //I3319     
    
      String.kmwEnableSupplementaryPlane(Ps && ((Ps['KS'] && (Ps['KS'] == 1)) || (Pk['KN'] == 'Hieroglyphic'))); // I3319 - SMP extension, I3363 (Build 301)
      keymanweb.saveCurrentKeyboard(Pk['KI'],Ps['KLC']);
    
      // Prepare and show the OSK for this keyboard
      osk._Load();

      // Remove the wait message, if defined
      if(typeof (util.wait) == 'function') util.wait(false);
    }
   
    // Execute any external (UI) code needed after loading keyboard
    keymanweb.doKeyboardLoaded(Pk['KI']);
    
    // Should not pending keyboard name here, as that prevents a subsequently 
    // selected keyboard from loading (KMW-101)
    //keymanweb._LoadingInternalName = null;
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
  keymanweb['KRS'] = keymanweb.KRS = function(Pstub)   
  {
    var Lk;
    
    // In initialization not complete, list the stub to be registered on completion of initialization
    if(!keymanweb['initialized'])
    {
      keymanweb.deferredKRS.push(Pstub); 
      return;
    }

    // The default stub is always the first keyboard stub loaded [and will be ignored by desktop browsers - not for beta, anyway]
    if(keymanweb.dfltStub == null)
    {
      keymanweb.dfltStub=Pstub;
      //if(device.formFactor == 'desktop') return 1;    //Needs further thought before release
    }

    // If no language code has been defined, and no stub has been registered for this keyboard, register with empty string as the language code
    if(typeof(Pstub['KLC']) == 'undefined') Pstub['KLC'] = '';
    if(typeof(Pstub['KL']) == 'undefined')  Pstub['KL'] = 'undefined';

    // If language code already defined (or not specified in stub), check to see if stub already registered
    for(Lk=0; Lk<keymanweb._KeyboardStubs.length; Lk++)
    {
      if(keymanweb._KeyboardStubs[Lk]['KI'] == Pstub['KI'])
      {
        if(Pstub['KLC'] == '' || (keymanweb._KeyboardStubs[Lk]['KLC'] == Pstub['KLC'])) return 1; // no need to register
      }
    }
   
    // Register stub (add to KeyboardStubs array)
    keymanweb._KeyboardStubs=keymanweb._push(keymanweb._KeyboardStubs,Pstub);
/*    
    // Sort stub array so that English is always first
    if(keymanweb._KeyboardStubs.length > 1) keymanweb._KeyboardStubs.sort(function(a,b){
      var ax,bx;
      switch(a['KLC'])
      {
        case 'eng': ax=0; break;
        case 'fra': ax=1; break;
        case 'deu': ax=2; break;
        default: ax=10; break;
      }
      switch(b['KLC'])
      {
        case 'eng': bx=0; break;
        case 'fra': bx=1; break;
        case 'deu': bx=2; break;
        default: bx=10; break;
      }
      return ax-bx;
      });
*/
    // TODO: Need to distinguish between initial loading of a large number of stubs and any subsequent loading.
    //   UI initialization should not be needed for each registration, only at end.
    // Reload this keyboard if it was the last active keyboard and 
    // make any changes needed by UI for new keyboard stub
    // (Uncommented for Build 360)
    keymanweb.doKeyboardRegistered(Pstub['KI'],Pstub['KL'],Pstub['KN'],Pstub['KLC']);
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
  
  keymanweb['KC'] = keymanweb.KC = function(n, ln, Pelem)
  {
    var v = keymanweb.cachedContext.get(n, ln);
    if(v !== null) return v;
    
    var r = keymanweb.KC_(n, ln, Pelem); 
    keymanweb.cachedContext.set(n, ln, r);
    return r;
  }
  
  /**
   * Function     KN    
   * Scope        Public
   * @param       {number}      n       Length of context to check
   * @param       {Object}      Pelem   Element to work with (must be currently focused element)
   * @return      {boolean}             True if length of context is less than or equal to n
   * Description  Test length of context, return true if the length of the context is less than or equal to n
   * 
   * Example     [abc|def] as INPUT, with the caret position marked by |:
   *             KN(3,Pelem) == TRUE
   *             KN(2,Pelem) == FALSE
   *             KN(4,Pelem) == TRUE
   */    
  keymanweb['KN'] = keymanweb.KN = function(n, Ptarg)    // KeyboardNul
  {
    var cx=this.KC(n+1, 1, Ptarg);
    if(cx === false) {
      // It appears that this can no longer be returned with KMW so probably should be removed
      // after testing
      return true;
    }
    
    // With #31, the result will be a replacement character if context is empty.
    return cx === "\uFFFE";
  }

  /**
   * Function     KCM   
   * Scope        Public
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {Object}      Ptarg   Focused element
   * @param       {string}      val     String to match
   * @param       {number}      ln      Number of characters to return
   * @return      {boolean}             True if selected context matches val
   * Description  Test keyboard context for match
   */    
  keymanweb['KCM'] = keymanweb.KCM = function(n, Ptarg, val, ln)  // Keyboard_ContextMatch 
  {             
    //KeymanWeb._Debug('KeymanWeb.KCM(n='+n+', Ptarg, val='+val+', ln='+ln+'): return '+(this.KC(n,ln,Ptarg)==val)); 
    var cx=this.KC(n, ln, Ptarg);
    if(cx !== false && cx === val) return true; // I3318
    this._DeadkeyResetMatched();                // I3318
    return false;
   }

  /**
   * Function     KIK      
   * Scope        Public
   * @param       {Object}  e   keystroke event
   * @return      {boolean}     true if keypress event
   * Description  Test if event as a keypress event
   */    
  keymanweb['KIK'] = keymanweb.KIK = function(e)          // Keyboard_IsKeypress 
  {                      
    if(keymanweb._ActiveKeyboard['KM'])    // I1380 - support KIK for positional layouts
      return !e.LisVirtualKey;             // will now return true for U_xxxx keys, but not for T_xxxx keys
    else  
      return keymanweb._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
    //if(e.charCode != 0) != null)
    // return e.charCode != 0;
    //return e.type == 'keypress';
  }
  
  /**
   * Function     KKM      
   * Scope        Public
   * @param       {Object}      e           keystroke event
   * @param       {number}      Lruleshift
   * @param       {number}      Lrulekey
   * @return      {boolean}                 True if key matches rule
   * Description  Test keystroke with modifiers against rule
   */    
  keymanweb['KKM'] = keymanweb.KKM = function(e,Lruleshift,Lrulekey)  // Keyboard_KeyMatch 
  { 
    var retVal = 0; // I3318
    var keyCode = (e.Lcode == 173 ? 189 : e.Lcode);  //I3555 (Firefox hyphen issue)

    if(e.vkCode > 255) keyCode = e.vkCode;           // added to support extended (touch-hold) keys for mnemonic layouts
      
    if(e.LisVirtualKey || keyCode > 255)            // added keyCode test for same reason
    {
      if((Lruleshift & 0x4000) == 0x4000 || (keyCode > 255))  // added keyCode test to support extended keys
      {
        retVal = ((Lrulekey == keyCode)  &&  ((Lruleshift&0x7F) == e.Lmodifiers)); //I3318, I3555
      }
    }
    else if((Lruleshift & 0x4000) == 0)
    {
      retVal = (keyCode == Lrulekey);         // I3318, I3555
    }
    if(!retVal) this._DeadkeyResetMatched();  // I3318
    return retVal;                            // I3318
  };
  
  /**
   * Function     KKI      
   * Scope        Public
   * @param       {number}      n
   * @param       {Object}      Ptarg
   * @param       {string}      val
   * @param       {number}      ln
   * @return      {Object}              Object with event's virtual key flag, key code, and modifiers
   * Description  Get object with extended key event information
   */    
  keymanweb['KKI'] = keymanweb.KKI = function(e)
  {
    var ei = {};
    ei['vk'] = e.LisVirtualKey; ei['code'] = e.Lcode; ei['modifiers'] = e.Lmodifiers;
    return ei;
  };
  
  /**
   * Function     KDM      
   * Scope        Public
   * @param       {number}      n       current cursor position
   * @param       {Object}      Ptarg   target element
   * @param       {number}      d       deadkey
   * @return      {boolean}             True if deadkey found selected context matches val
   * Description  Match deadkey at current cursor position
   */    
  keymanweb['KDM'] = keymanweb.KDM = function(n, Ptarg, d)
  {                              
    if(keymanweb._DeadKeys.length == 0) return false; // I3318  
  
    var sp=keymanweb._SelPos(Ptarg); 
    n = sp - n;   
    for(var i = 0; i < keymanweb._DeadKeys.length; i++)
      if(keymanweb._DeadKeys[i].p == n  &&  keymanweb._DeadKeys[i].d == d) {
        keymanweb._DeadKeys[i].matched = 1; return 1; // I3318        
      }
    this._DeadkeyResetMatched();                      // I3318

    return false;
  }
  
  /**
   * Function     KBR      
   * Scope        Public
   * Description  Reset/terminate beep or flash (not currently used: Aug 2011)
   */    
  keymanweb['KBR'] = keymanweb.KBR = function() // KeyboardBeepReset
  {
    keymanweb.cachedContext.reset();
    
    var Lbo;
    keymanweb._BeepTimeout = 0;
    for(Lbo=0;Lbo<keymanweb._BeepObjects.length;Lbo++)  // I1511 - array prototype extended
    {
      keymanweb._BeepObjects[Lbo].e.style.backgroundColor = keymanweb._BeepObjects[Lbo].c;
    }
    keymanweb._BeepObjects = [];
  }
    
  /**
   * Function     KB      
   * Scope        Public
   * @param       {Object}      Pelem     element to flash
   * Description  Flash body as substitute for audible beep
   */    
  keymanweb['KB'] = keymanweb.KB = function(Pelem)    // Keyboard_Beep
  {    
    keymanweb.cachedContext.reset();
    
    if(Pelem.body) Pelem=Pelem.body; // I1446 - beep sometimes fails to flash when using OSK and rich control
    
    if(!Pelem.style || typeof(Pelem.style.backgroundColor)=='undefined') return;

    for(var Lbo=0;Lbo<keymanweb._BeepObjects.length;Lbo++)   // I1446 - beep sometimes fails to return background color to normal
    {                                                     // I1511 - array prototype extended
      if(keymanweb._BeepObjects[Lbo].e == Pelem) return;
    }
    
    keymanweb._BeepObjects=keymanweb._push(keymanweb._BeepObjects,{e:Pelem, c:Pelem.style.backgroundColor});
    Pelem.style.backgroundColor = '#000000';
    if(keymanweb._BeepTimeout == 0)
    {
      keymanweb._BeepTimeout = 1;
      window.setTimeout(keymanweb.KBR, 50);
    }
    //Pelem.style.backgroundColor = Lcolour;
  }
  
  /**
   * Function     KA      
   * Scope        Public
   * @param       {number}      n     character position (index) 
   * @param       {string}      ch    character to find in string
   * @param       {string}      s     'any' string   
   * @return      {boolean}           True if character found in 'any' string, sets index accordingly
   * Description  Test for character matching
   */    
  keymanweb['KA'] = keymanweb.KA = function(n,ch,s)  // Keyboard_Any()
  {   
    if(ch == '') return 0;
    var Lix = s._kmwIndexOf(ch); //I3319
    keymanweb._AnyIndices[n] = Lix;
    return Lix >= 0;
  }
  
  /**
   * Function     KO      
   * Scope        Public
   * @param       {number}      dn      number of characters to overwrite
   * @param       {Object}      Pelem   element to output to 
   * @param       {string}      s       string to output   
   * Description  Keyboard output
   */    
  keymanweb['KO'] = keymanweb.KO = function(dn, Pelem, s) // Keyboard_Output()
  {
    keymanweb.cachedContext.reset();
        
    // KeymanTouch for Android uses direct insertion of the character string
    if('oninserttext' in keymanweb)
    {
      keymanweb['oninserttext'](dn,s); 
    }   
   
    if(Pelem.body) var Ldoc=Pelem; else var Ldoc=Pelem.ownerDocument;	// I1481 - integration with rich editors not working 100%
    var Li, Ldv;
   
    if(Pelem.className.indexOf('keymanweb-input') >= 0) 
    {
      var t=keymanweb.getTextBeforeCaret(Pelem);
      if(dn > 0) t=t._kmwSubstr(0,t._kmwLength()-dn)+s; else t=t+s;
      keymanweb.setTextBeforeCaret(Pelem,t);
      return;
    }
   
    if (keymanweb.legacy)
    { 
      if(dn>0)
        Pelem.value=Pelem.value._kmwSubstr(0,Pelem.value._kmwLength()-dn)+s;  //I3319
      else
        Pelem.value=Pelem.value+s;
    }
    else if (Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&  
        (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')      
      )  // I2457 - support contentEditable elements in mozilla, webkit
    {
      /* Editable iframe and contentEditable elements for mozilla */
      var _IsEditableIframe = Ldoc.designMode.toLowerCase() == 'on';
      if(_IsEditableIframe) var _CacheableCommands = keymanweb._CacheCommands(Ldoc);
     
      var Lsel = Ldv.getSelection();
      var LselectionStart = Lsel.focusNode.nodeValue ? Lsel.focusNode.substringData(0,Lsel.focusOffset)._kmwLength() : 0;  // I3319
      
      if(!Lsel.isCollapsed) Lsel.deleteFromDocument();  // I2134, I2192
      //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

      var Lrange = Lsel.getRangeAt(0);
      if(dn > 0) { 
        Lrange.setStart(Lsel.focusNode, Lsel.focusOffset - Lsel.focusNode.nodeValue.substr(0,Lsel.focusOffset)._kmwSubstr(-dn).length); // I3319
        Lrange.deleteContents(); 
      }

      //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

      if(s._kmwLength() > 0)  // I2132 - exception if s.length > 0, I3319
      {
        if(Lsel.focusNode.nodeType == 3)
        {
					// I2134, I2192
          // Already in a text node
          //KeymanWeb._Debug('KO: Already in a text node, adding "'+s+'": '+Lsel.focusOffset + '-> '+Lsel.toString());
          var LfocusOffset = Lsel.focusOffset;
          //KeymanWeb._Debug('KO: node.text="'+Lsel.focusNode.data+'", node.length='+Lsel.focusNode.length);
          Lsel.focusNode.insertData(Lsel.focusOffset, s);
          try
          {
            Lsel.extend(Lsel.focusNode, LfocusOffset + s.length); 
          }
          catch(e)
          {
            // Chrome (through 4.0 at least) throws an exception because it has not synchronised its content with the selection.  scrollIntoView synchronises the content for selection
            Lsel.focusNode.parentNode.scrollIntoView();
            Lsel.extend(Lsel.focusNode, LfocusOffset + s.length);
          }
        }
        else
        {
          // Create a new text node - empty control
          //KeymanWeb._Debug('KO: Creating a new text node for "'+s+'"');
          var n = Ldoc.createTextNode(s);
          Lrange.insertNode(n);
          Lsel.extend(n,s.length);
        }
      }
      if(_IsEditableIframe) keymanweb._CacheCommandsReset(Ldoc, _CacheableCommands, null);// I2457 - support contentEditable elements in mozilla, webkit
      
      Lsel.collapseToEnd();

      // Adjust deadkey positions 
      if(dn >= 0)
      {
        keymanweb._DeadkeyDeleteMatched();                                  // I3318
        keymanweb._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318
      }
    }
    
    // Internet Explorer   (including IE9)   
    else if(Ldoc  &&  (Ldv=Ldoc.selection)) // build 77 - use elem.ownerDocument.selection
    { 
      if(Ldoc.body.isContentEditable || Ldoc.designMode.toLowerCase()=='on')  // I1295 - isContentEditable
      {
        var _CacheableCommands = keymanweb._CacheCommands(Ldoc);
      }
   
      var Lrange = Ldv.createRange(), Ls1;
      if(Lrange.text != '') 
      {
        Ldv.clear();
        dn = 0;
      }
      else Lrange.collapse(true);

      if(dn > 0) {              
        Lrange.moveStart('character',-2*dn);  // I3319 (next four lines
        var s0=Lrange.text,s1=s0._kmwSubstr(-dn);
        Lrange.collapse(false); //move start back to end
        Lrange.moveStart('character',-s1.length);
      }
      else dn = 0;

      Lrange.text = s;

      if(Ldoc.body.isContentEditable || Ldoc.designMode.toLowerCase()=='on') // I1295 - isContentEditable
      {
        Lrange.moveStart('character',-s.length);
        
        keymanweb._CacheCommandsReset(Ldoc, _CacheableCommands,Lrange.select);
        Lrange.moveStart('character',s.length);
        Lrange.select();
      }
      // Adjust deadkey positions 
      if(dn >= 0)
      {
        keymanweb._DeadkeyDeleteMatched();                                  // I3318
        keymanweb._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318
      }
 
      keymanweb._Selection = Ldv.createRange();
      keymanweb._Selection.select();
      keymanweb._Selection.scrollIntoView();
    }    

    // Mozilla et al; IE9+ also recognizes setSelectionRange, but does not seem to work in exactly the same way as Mozilla
    else if (Pelem.setSelectionRange)
    {                                        
      var LselectionStart, LselectionEnd;
            
      if(Pelem._KeymanWebSelectionStart != null) // changed to allow a value of 0
      {
        LselectionStart = Pelem._KeymanWebSelectionStart;
        LselectionEnd = Pelem._KeymanWebSelectionEnd;
      }
      else
      {
        LselectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart);  // I3319
        LselectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);      // I3319
      }
      
      var LscrollTop, LscrollLeft;
      if(Pelem.type.toLowerCase() == 'textarea' && typeof(Pelem.scrollTop) != 'undefined') {
        LscrollTop = Pelem.scrollTop; LscrollLeft = Pelem.scrollLeft;
      }

      if(dn < 0) // Don't delete, leave context alone (dn = -1)
      {
        Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart) + s + Pelem.value._kmwSubstring(LselectionEnd);    //I3319
        dn = 0;
      }
      else if(LselectionStart < dn)
        Pelem.value = s + Pelem.value._kmwSubstring(LselectionEnd); //I3319
      else
        Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart-dn) + s + Pelem.value._kmwSubstring(LselectionEnd); //I3319

      // Adjust deadkey positions 
      if(dn >= 0)
      {
        keymanweb._DeadkeyDeleteMatched(); // I3318
        keymanweb._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318,I3319
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
    if(typeof(keymanweb.refreshElementContent) == 'function') keymanweb.refreshElementContent(Pelem);    
  }
  
  /**
   * Function     KDO      
   * Scope        Public
   * @param       {number}      Pdn     no of character to overwrite (delete) 
   * @param       {Object}      Pelem   element to output to 
   * @param       {number}      Pd      deadkey id
   * Description  Record a deadkey at current cursor position, deleting Pdn characters first
   */    
  keymanweb['KDO'] = keymanweb.KDO = function(Pdn,Pelem,Pd)
  {               
    keymanweb.cachedContext.reset();             
    var Lc = new Object(); 
    if(Pdn >= 0) keymanweb.KO(Pdn,Pelem,"");  //I3318 corrected to >=
    Lc.p=keymanweb._SelPos(Pelem); Lc.d=Pd;                 
    keymanweb._DeadKeys=keymanweb._push(keymanweb._DeadKeys,Lc);        
             
//    _DebugDeadKeys(Pelem, 'KDeadKeyOutput: dn='+Pdn+'; deadKey='+Pd);
  }

  /**
   * Function     KIO      
   * Scope        Public
   * @param       {number}      Pdn     no of character to overwrite (delete) 
   * @param       {string}      Ps      string
   * @param       {numebr}      Pn      index
   * @param       {Object}      Pelem   element to output to 
   * Description  Output a character selected from the string according to the offset in the index array
   */    
  keymanweb['KIO'] = keymanweb.KIO = function(Pdn,Ps,Pn,Pelem)
  {
    keymanweb.cachedContext.reset();
    if(keymanweb._AnyIndices[Pn-1] < Ps._kmwLength())                      //I3319        
      keymanweb.KO(Pdn,Pelem,Ps._kmwCharAt(keymanweb._AnyIndices[Pn-1]));  //I3319
  }
  
/**
 * Function     _CacheCommands
 * Scope        Private
 * @param       {Object}    _Document
 * @return      {Array.<string>}        List of style commands that are cacheable
 * Description  Build reate list of styles that can be applied in iframes
 */    
  keymanweb._CacheCommands = function(_Document) // I1204 - style application in IFRAMEs, I2192, I2134, I2192   
  {
    //var _CacheableBackColor=(_Document.selection?'hilitecolor':'backcolor');
    var _CacheableCommands=[['backcolor',1],['fontname',1],['fontsize',1],['forecolor',1],['bold',0],['italic',0],['strikethrough',0],['subscript',0],['superscript',0],['underline',0]];
    if(_Document.defaultView) keymanweb._push(_CacheableCommands,['hilitecolor',1]);
      
    for(var n=0;n < _CacheableCommands.length; n++)  // I1511 - array prototype extended
    {
      //KeymanWeb._Debug('Command:'+_CacheableCommands[n][0]);
      keymanweb._push(_CacheableCommands[n],_CacheableCommands[n][1] ? _Document.queryCommandValue(_CacheableCommands[n][0]) : _Document.queryCommandState(_CacheableCommands[n][0]));
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
  keymanweb._CacheCommandsReset = function(_Document, _CacheableCommands, _func)
  {
    for(var n=0;n < _CacheableCommands.length; n++)  // I1511 - array prototype extended
    {
      //KeymanWeb._Debug('ResetCacheCommand:'+_CacheableCommands[n][0]+'='+_CacheableCommands[n][2]);
      if(_CacheableCommands[n][1])
      {
        if(_Document.queryCommandValue(_CacheableCommands[n][0]) != _CacheableCommands[n][2])
        {
          if(_func)_func();
          _Document.execCommand(_CacheableCommands[n][0], false, _CacheableCommands[n][2]);
        }
      }
      else if(_Document.queryCommandState(_CacheableCommands[n][0]) != _CacheableCommands[n][2])
      {if(_func)_func();
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
  keymanweb['KIFS'] = keymanweb.KIFS = function(systemId,strValue,Pelem)
  {     
    var result=true;             
    if(systemId == keymanweb.TSS_LAYER) 
      result = (osk.layerId === strValue);
    else if(systemId == keymanweb.TSS_PLATFORM)
    {
      var i,constraint,constraints=strValue.split(' ');
      for(i=0; i<constraints.length; i++)
      {
        constraint=constraints[i].toLowerCase();
        switch(constraint)
        {
          case 'touch':
          case 'hardware':
            if(device.touchable != (constraint == 'touch')) result=false;
        }
        switch(constraint)
        {
          case 'windows':
          case 'android':
          case 'ios':
          case 'macosx':
          case 'linux':
            if(device.OS.toLowerCase() != constraint) result=false;
        }
        switch(constraint)
        {
          case 'tablet':
          case 'phone':
          case 'desktop':
            if(device.formFactor != constraint) result=false;
        }
        switch(constraint)
        {
          case 'web':
            if(device.browser == 'native') result=false; // web matches anything other than 'native'
            break;
          case 'native':
          case 'ie':
          case 'chrome':
          case 'firefox':
          case 'safari':
          case 'opera':
            if(device.browser != constraint) result=false;  
        }
      }
    }  
    return result;    //Moved from previous line, now supports layer selection, Build 350 
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
  keymanweb['KSETS'] = function(systemId,strValue,Pelem)
  {
    keymanweb.cachedContext.reset();
    if(systemId == keymanweb.TSS_LAYER)
      return osk.showLayer(strValue);     //Buld 350, osk reference now OK, so should work
    else
      return false;
  }
 
  /**
   * Load an option store value from a cookie or default value
   * 
   * @param       {string}      kbdName     keyboard internal name
   * @param       {string}      storeName   store (option) name, embedded in cookie name
   * @param       {string}      dfltValue   default value
   * @return      {string}                  current or default option value   
   */    
  keymanweb['KLOAD'] = function(kbdName,storeName,dfltValue)
  {
    keymanweb.cachedContext.reset();
    var cName='KeymanWeb_'+kbdName+'_Option_'+storeName,cValue=util.loadCookie(cName);
    if(typeof cValue[storeName] != 'undefined') 
      return unescape(cValue[storeName]);
    else
      return dfltValue;
  }

  /**
   * Save an option store value to a cookie 
   * 
   * @param       {string}      storeName   store (option) name, embedded in cookie name
   * @param       {string}      optValue    option value to save
   * @return      {boolean}                 true if save successful
   */    
  keymanweb['KSAVE'] = function(storeName,optValue)
  {
    keymanweb.cachedContext.reset();
    var kbd=keymanweb._ActiveKeyboard;
    if(!kbd || typeof kbd['KI'] == 'undefined' || kbd['KI'] == '') return false;
    
    var cName='KeymanWeb_'+kbd['KI']+'_Option_'+storeName,cValue=escape(optValue);

    util.saveCookie(cName,cValue);
    return true;
  }

 /**
  * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
  */
  keymanweb['GetLastActiveElement'] = function() { return keymanweb._LastActiveElement; }
  keymanweb['FocusLastActiveElement'] = function() { keymanweb._FocusLastActiveElement(); }

  //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
  keymanweb['HideHelp'] = function() {osk._Hide(true);}
  keymanweb['ShowHelp'] = function(Px,Py) {osk._Show(Px,Py);}  
  keymanweb['ShowPinnedHelp'] = function() {osk.userPositioned=true; osk._Show(-1,-1);}
  
  /**
   * Cache of context storing and retrieving return values from KC
   * Must be reset prior to each keystroke and after any text changes
   * MCD 3/1/14   
   **/         
  keymanweb.cachedContext = {
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
  
})();  
