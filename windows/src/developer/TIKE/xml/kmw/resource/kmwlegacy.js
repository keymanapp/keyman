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

/**
 * Declare KeymanWeb variable as the tavultesoft.keymanweb.legacy object
 */
var KeymanWeb = tavultesoft['keymanweb']['legacy'] = {};

window['KeymanWeb'] = KeymanWeb;

KeymanWeb['build'] = __BUILD__;
KeymanWeb['version'] = '1.0';
KeymanWeb['HelpURL'] = tavultesoft['keymanweb']['helpURL'];

// Declare external legacy function names as calls to internal functions
(function(keymanweb) 
{
  // Declare keymanweb osk, legacy and util objects
  var osk=keymanweb['osk'],legacy=keymanweb['legacy'],util=keymanweb['util'];
  
  if(typeof(window['KeymanWeb_onkeyboardinstalled']) != 'undefined') 
    keymanweb['addEventListener']('keyboardregistered',window['KeymanWeb_onkeyboardinstalled']);

  if(typeof(window['KeymanWeb_onkeyboardloaded']) != 'undefined') 
    keymanweb['addEventListener']('keyboardloaded',window['KeymanWeb_onkeyboardloaded']);   
    
  if(typeof(window['KeymanWeb_onkeyboardchange']) != 'undefined') 
    keymanweb['addEventListener']('keyboardchange',window['KeymanWeb_onkeyboardchange']);   

  /**
   * Function    AttachToControl
   * Scope       Public
   * Parameters  Pelem       Element to which KMW will be attached
   * Returns     None
   * Description Attaches KMW to control (or IFrame) 
   */  
  legacy['AttachToControl'] = function(Pelem)
  {
    keymanweb.AttachToControl(Pelem);
  }

  /**
   * Function    IsHelpVisible
   * Scope       Public   
   * Parameters  None
   * Returns     True if KMW Help displayed
   * Description Test if KMW Help is visible 
   */    
  legacy['IsHelpVisible'] = function()
  {
    return osk._Visible;
  }
  
  /**
   * Function    GetEnabled
   * Scope       Public   
   * Parameters  None
   * Returns     True if KMW enabled
   * Description Test if KMW enabled 
   */    
  legacy['GetEnabled'] = function()
  {
    return keymanweb._Enabled;
  }
  
  /**
   * Function    SetEnabled
   * Scope       Public   
   * Parameters  Pvalue   True to enable KMW
   * Returns     Nothing
   * Description Enable or disable KMW
   */    
  legacy['SetEnabled'] = function(Pvalue)
  {
    keymanweb.SetEnabled(Pvalue);
  }

  /**
   * Function    ShowHelpAuto
   * Scope       Public   
   * Parameters  None
   * Returns     None
   * Description Automatically position KMW Help relative to element
   */    
    legacy['ShowHelpAuto'] = function()
  {
    osk._UserLocated=false;
    osk._Show();
  }
     
  /**
   * Function    ShowPinnedHelp
   * Scope       Public   
   * Parameters  None
   * Returns     None
   * Description Fix position of KMW Help at current position
   */    
  legacy['ShowPinnedHelp'] = function()
  {
    osk._UserLocated=true;
    osk._Show(-1,-1);
  }

  /**
   * Function    DisableControl
   * Scope       Public   
   * Parameters  Pelem   Element to be disabled
   * Returns     None
   * Description Disable KMW control element 
   */    
  legacy['DisableControl'] = function(Pelem)
  {
    keymanweb.DisableControl(Pelem);
  }

  /**
   * Function    EnableControl
   * Scope       Public   
   * Parameters  Pelem   Element to be enabled
   * Returns     None
   * Description Enable KMW control element 
   */    
  legacy['EnableControl'] = function(Pelem)
  {
    keymanweb.EnableControl(Pelem);
  }
  
  /**
   * Function    SetDefaultKeyboardForControl
   * Scope       Public   
   * Parameters  Pelem    Control element 
   *             Pkbd     Keyboard   
   * Returns     None
   * Description Set default keyboard for control 
   */    
  legacy['SetDefaultKeyboardForControl'] = function(Pelem,Pkbd)
  {
      keymanweb.SetDefaultKeyboardForControl(Pelem,Pkbd);
  }

  /**
   * Function    GetHelpPos
   * Scope       Public   
   * Parameters  None
   * Returns     Array with Help window position and size
   * Description Get position and size of Help window 
   */    
  legacy['GetHelpPos'] = function()
  {
    return osk['GetPos']();
  }
  
  /**
   * Function    SetHelpPos
   * Scope       Public   
   * Parameters  Px   x-coordinate for Help rectangle
   *             Py   y-coordinate for Help rectangle 
   * Returns     None
   * Description Set position of Help window 
   */    
  legacy['SetHelpPos'] = function(Px,Py)
  {
    osk['SetPos'](Px,Py);
  }

  /**
   * Function    SetHelpSize
   * Scope       Public   
   * Parameters  Pw   width of Help window
   * Returns     None
   * Description Set width of Help window 
   */    
  legacy['SetHelpSize'] = function(Pw)
  {
    osk['SetSize'](Pw);
  }  
  
  /**
   * Function    HelpBox (temporary)
   * Scope       Private 
   * Parameters  None
   * Returns     OnScreen Keyboard container DIV element
   * Description Lets UI get or set OSK properties
   */
  legacy['HelpBox'] = function()
  {
    return osk._Box;  
  }
                     
  /**
   * Function    FocusLastActiveElement
   * Scope       Public   
   * Parameters  None
   * Returns     None
   * Description Set focus to last active target element
   */    
  legacy['FocusLastActiveElement'] = function()
  {
      keymanweb._FocusLastActiveElement();
  }
  
  /**
   * Function    GetLastActiveElement
   * Scope       Public   
   * Parameters  None
   * Returns     Last active element 
   * Description Return the last target element active (before KMW activated)
   */    
  legacy['GetLastActiveElement'] = function()
  {
    return keymanweb._LastActiveElement;
  }

  /**
   * Function    SetLastActiveElement
   * Scope       Public   
   * Parameters  e Element
   * Returns     None
   * Description Redefines (or clears) the last active target element 
   */    
  legacy['SetLastActiveElement'] = function(e)
  {
    keymanweb._LastActiveElement = e;
  }
 
 /**
   * Function    SetActiveKeyboard
   * Scope       Public
   * Parameters  PInternalName
   * Returns     Nothing
   * Description Change active keyboard by (internal) keyboard name
   */    
  legacy['SetActiveKeyboard'] = function(PInternalName)
  {
    keymanweb['SetActiveKeyboard'](PInternalName);
  }
  
  /**
   * Function    GetActiveKeyboard
   * Scope       Public
   * Parameters  None
   * Returns     Name of active keyboard
   * Description Return internal name of currently active keyboard
   */    
  legacy['GetActiveKeyboard'] = function()
  {
    return keymanweb['GetActiveKeyboard']();
  }
  
  /**
   * Function    GetKeyboardDetail
   * Scope       Public
   * Parameters  PInternalName    Internal name of keyboard
   * Returns     Details of named keyboard
   * Description Return keyboard details
   */    
  legacy['GetKeyboardDetail'] = function(PInternalName)  
  {
    return keymanweb.GetKeyboardDetail(PInternalName);
  } 

  /**
   * Function    GetKeyboards
   * Scope       Public
   * Parameters  None
   * Returns     Array of installed keyboard object details
   * Description Return array of installed keyboard details
   */    
  legacy['GetKeyboards'] = function()
  {
    return keymanweb['GetKeyboards']();
  }

  /**
   * Function    HelpIsPinned
   * Scope       Public
   * Parameters  none
   * Returns     true if pinned
   * Description Test if Help window is pinned  
   */    
  legacy['HelpIsPinned'] = function()
  {
    return osk._UserLocated;
  }
  
  /**
   * Function    Init
   * Scope       Public
   * Parameters  None
   * Returns     Nothing
   * Description Initialize KMW window  
   */    
  legacy['Init'] = function()
  {
    var opt={};
    // Legacy variable initialization
    if(typeof(window['KeymanWeb_Root']) == 'undefined') 
      opt['root']=window['KeymanWeb_Root']; 
    if(typeof(window['KeymanWeb_AttachType']) == 'undefined') 
      opt['attachType']=window['KeymanWeb_AttachType'];
    if(typeof(window['KeymanWeb_ControlDownColor']) == 'undefined') 
      opt['controlDownColor']=window['KeymanWeb_ControlDownColor'];
    if(typeof(window['KeymanWeb_KeyDownColor']) == 'undefined') 
      opt['keyDownColor']=window['KeymanWeb_KeyDownColor'];
    if(typeof(window['KeymanWeb_KeyHoverColor']) == 'undefined') 
      opt['keyHoverColor']=window['KeymanWeb_KeyHoverColor'];
    if(typeof(window['KeymanWeb_DefaultKeyboardName']) == 'undefined') 
      opt['defaultKeyboardName']=window['KeymanWeb_DefaultKeyboardName'];
    if(typeof(window['KeymanWeb_DefaultKeyboardHelp']) == 'undefined') 
      opt['defaultKeyboardHelp']=window['KeymanWeb_DefaultKeyboardHelp'];
  
    keymanweb['init'](opt);
  }
    
  /**
   * Function    GetAbsoluteX
   * Scope       Public
   * Parameters  Pobj
   * Returns     Absolute x-coordinate (needed for iFrames).
   * Description Get absolute x-coordinate of Pobj element
   */    
  legacy['GetAbsoluteX'] = function(Pobj) 
  {
    return util._GetAbsoluteX(Pobj);
  }

/**
 * Function    GetAbsoluteY
 * Scope       Public
 * Parameters  Pobj
 * Returns     Absolute y-coordinate (needed for iFrames).
 * Description Get absolute y-coordinate of Pobj element
 */    
  legacy['GetAbsoluteY'] = function(Pobj)
  {
    return util._GetAbsoluteY(Pobj);
  }

  /**
   * Function    AddStyleSheet
   * Scope       Public
   * Parameters  s    UI stylesheet
   * Returns     Nothing
   * Description Add style to KMW UI
   */      
  legacy['AddStyleSheet'] = function(s)
  {
    util['AddStyleSheet'](s);
  }
 
 /**
  * Visual Keyboard Legacy Functions
  */
  
  /**
   * Function    Help
   * Scope       Public   
   * Parameters  None
   * Returns     None 
   * Description Show or hide KMW Help window below the KMW window
   *             (Should never be used except by old floating UI)
   *             No longer called by float UI. Cleaned up and kept here, 
   *             but will almost certainly never be needed any more.      
   */    
  legacy['Help'] = function()
  {
    if(osk._Visible) osk._Hide(true); else osk._Show();
    keymanweb['FocusLastActiveElement']();
  }
     
  /**
   * Function    GetHelpRect
   * Scope       Public   
   * Parameters  None
   * Returns     Rectangle containing KMW Virtual Keyboard
   * Description Get rectangle containing KMW Virtual Keyboard 
   */    
  legacy['GetHelpRect'] = function()	
  {
    return osk['GetRectOSK']();
  }
  
  /**
   * Function    BuildVisualKeyboard
   * Scope       Public
   * Parameters  PInternalName   Keyboard name
   *             PStatic         true if OSK to be unselectable         
   * Returns     Modified (html-safe) string
   * Description Encode angle brackets and ampersand in text string
   */    
  legacy['BuildVisualKeyboard'] = function(PInternalName, PStatic)
  {
    return osk.BuildVisualKeyboard(PInternalName,PStatic);
  }

  /**
   * Function    ShowHelp
   * Scope       Public   
   * Parameters  Px   x-coordinate for Help rectangle
   *             Py   y-coordinate for Help rectangle 
   * Returns     None
   * Description Display KMW Help (OSK) at specified position 
   */    
   legacy['ShowHelp'] = function(Px, Py)
  {
    osk._Show(Px, Py);
  }
  
  /**
   * Function    HideHelp
   * Scope       Public   
   * Parameters  None
   * Returns     None
   * Description Hide KMW Help window 
   */    
  legacy['HideHelp'] = function()
  {
    // Must assert 'hiddenByUser' to ensure correct behaviour with CJK keyboard OSKs
    osk._Hide(true);
  }

 /**
   * Function    CreateShim
   * Scope       Public
   * Parameters  None
   * Returns     iFrame element
   * Description Create an element to go between KMW and drop down (to fix IE6 bug)
   */    
  legacy['CreateShim'] = function()     
  {
    return util['CreateShim']();
  }
  
  /**
   * Function    ShowShim
   * Scope       Public
   * Parameters  Pvkbd    Visual keyboard DIV element
   *             Pframe   iframe for Visual Keyboard
   *             Phelp    OSK Help DIV element 
   * Returns     None
   * Description Display iFrame under OSK at its currently defined position, to allow OSK to overlap SELECT elements (IE6 fix)  
   */    
  legacy['ShowShim'] = function(Pvkbd,Pframe,Phelp)     
  {
    util['ShowShim'](Pvkbd,Pframe,Phelp);
  }

  
  /**
   * Function    HideShim
   * Scope       Public
   * Parameters  Pframe   iframe for Visual Keyboard
   * Returns     None
   * Description Hide iFrame containing OSK 
   */    
  legacy['HideShim'] = function(Pframe)
  {
    util['HideShim'](Pframe)
  }

  /**
   * Function      OSKShim
   * Scope         Public
   * Parameters    None
   * Returns       Container DIV for OSK shim
   * Description  Lets user interface get or set properties of OSK shim element
   */            
  legacy['OSKShim'] = function()
  {
    return osk._Shim;
  }
  
  /**
   * Function    KSF
   * Scope       Public
   * Parameters  None
   * Returns     Nothing
   * Description Save keyboard focus
   */    
  legacy['KSF'] = function()     // KeyboardSaveFocus
  {
    keymanweb.KSF();
  }
  
  /**
   * Function    KT
   * Scope       Public
   * Parameters  Ptext    Text to insert
   *             PdeadKey Dead key value, if any (???)
   * Returns     true if inserted
   * Description Insert text into active control
   */    
  legacy['KT'] = function(Ptext,PdeadKey)  // KeyboardInsertText
  {
    return keymanweb.KT(Ptext,PdeadKey);
  }  
  
  /**
   * Function    KR
   * Scope       Public
   * Parameters  Pk     Keyboard
   * Returns     Nothing
   * Description Register keyboard
   */    
  legacy['KR'] = function(Pk)
  {
    return keymanweb.KR(Pk);
  }

  /**
   * Function    KRS
   * Scope       Public
   * Parameters  Pstub    Keyboard stub
   * Returns     1 if already loaded, else null
   * Description Insert text into active control
   */    
  legacy['KRS'] = function(Pstub)  /* KeyboardRegisterStub */
  { 
    return keymanweb.KRS(Pstub);
  }
  
  /**
   * Function    KC    
   * Scope       Public
   * Parameters  n     Number of characters to move back from caret
   *             ln    Number of characters to return
   *             Pelem Element to work with (must be currently focused element)
   * Returns     String of characters being the context of that range
   * Description Return keyboard context context for a specified range, relative to caret
   * 
   * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
   *             KC(2,1,Pelem) == "e"
   *             KC(3,3,Pelem) == "def"
   *             KC(10,10,Pelem) == "abcdef"
   */    
  
  legacy['KC'] = function(n, ln, Pelem) /* KeyboardContext */
  {
    return keymanweb.KC(n,ln,Pelem);
  }

  /**
   * Function    KN    
   * Scope       Public
   * Parameters  n     Length of context to check
   *             Pelem Element to work with (must be currently focused element)
   * Returns     TRUE if length of context is less than or equal to n
   * Description Returns TRUE if the length of the context is less than or equal to n
   * 
   * Example     [abc|def] as INPUT, with the caret position marked by |:
   *             KN(3,Pelem) == TRUE
   *             KN(2,Pelem) == FALSE
   *             KN(4,Pelem) == TRUE
   */    
  legacy['KN'] = function(n, Ptarg)    // KeyboardNul
  {
    return keymanweb.KN(n,Ptarg);
  }

  /**
   * Function    KCM   
   * Scope       Public
   * Parameters  n
   *             Ptarg
   *             val
   *             ln
   * Returns     TRUE if selected context matches val
   * Description Test keyboard context for match
   */    
  legacy['KCM'] = function(n, Ptarg, val, ln)  // Keyboard_ContextMatch
  {
    return keymanweb.KCM(n,Ptarg,val,ln);
  } 

  /**
   * Function    KIK      
   * Scope       Public
   * Parameters  e
   * Returns     TRUE if keypress event
   * Description Test if event as a keypress event
   */    
  legacy['KIK'] = function(e)          // Keyboard_IsKeypress
  {
    return keymanweb.KIK(e);
  } 

  /**
   * Function    KKM      
   * Scope       Public
   * Parameters  e          keystroke event
   *             Lruleshift
   *             Lrulekey
   * Returns     TRUE if key matches rule
   * Description Test keystroke with modifiers against rule
   */    
  legacy['KKM'] = function(e,Lruleshift,Lrulekey)  // Keyboard_KeyMatch
  { 
    return keymanweb.KKM(e,Lruleshift,Lrulekey);
  } 

  /**
   * Function    KKI      
   * Scope       Public
   * Parameters  n
   *             Ptarg
   *             val
   *             ln
   * Returns     Object with event's virtual key flag, key code, and modifiers
   * Description Extended key event information
   */    
  legacy['KKI'] = function(e)
  { 
    return keymanweb.KKI(e);
  }
  
  /**
   * Function    KIF
   * Scope       Public
   * Parameters  store1     String content of store 1 to compare
   *             store2     String content of store 2 to compare to
   *             mode       1 = compare inequality; 2 = compare equality   
   *             Pelem      Currently active element      
   * Returns     True if the test succeeds 
   * Description KIF compares the values of two stores, with binary equality.
   */    
  legacy['KIF'] = function(store1,store2,mode,Pelem)
  {
    return keymanweb.KIF(store1,store2,mode,Pelem);
  }                

  /**
   * Function    KDM      
   * Scope       Public
   * Parameters  n      current cursor position
   *             Ptarg  target element
   *             d      deadkey
   * Returns     TRUE if deadkey found selected context matches val
   * Description Match deadkey at current cursor position
   */    
  legacy['KDM'] = function(n, Ptarg, d)
  {
    return keymanweb.KDM(n,Ptarg,d);
  }

  /**
   * Function    KBR      
   * Scope       Public
   * Parameters  None
   * Returns     Nothing
   * Description Reset/terminate beep or flash
   */    
  legacy['KBR'] = function() // KeyboardBeepReset
  {
    keymanweb.KBR();
  }

  /**
   * Function    KB      
   * Scope       Public
   * Parameters  Pelem    element to flash
   * Returns     Nothing
   * Description Flash body as substitute for audible beep
   */    
  legacy['KB'] = function(Pelem)    // Keyboard_Beep()
  {
    keymanweb.KB(Pelem);
  }

  /**
   * Function    KA      
   * Scope       Public
   * Parameters  n    character position (index) 
   *             ch   character to find in string
   *             s    'any' string   
   * Returns     True if character found in 'any' string, sets index accordingly
   * Description Test for character matching
   */    
  legacy['KA'] = function(n,ch,s)  // Keyboard_Any()
  {
    return keymanweb.KA(n,ch,s);
  }

  /**
   * Function    KO      
   * Scope       Public
   * Parameters  dn     number of characters to overwrite
   *             Pelem  element to output to 
   *             s      string to output   
   * Returns     Nothing
   * Description Keyboard output
   */    
  legacy['KO'] = function(dn, Pelem, s) // Keyboard_Output()
  {
    keymanweb.KO(dn,Pelem,s);
  }

  /**
   * Function    KDO      
   * Scope       Public
   * Parameters  Pdn    no of character to overwrite (delete) 
   *             Pelem  element to output to 
   *             Pd     deadkey id
   * Returns     Nothing
   * Description Output a deadkey at current cursor position, deleting Pdn characters first
   */    
  legacy['KDO'] = function(Pdn,Pelem,Pd)
  {
    keymanweb.KDO(Pdn,Pelem,Pd);
  }

  /**
   * Function    KIO      
   * Scope       Public
   * Parameters  Pdn    no of character to overwrite (delete) 
   *             Ps     string
   *             Pn     index
   *             Pelem  element to output to 
   * Returns     Nothing
   * Description Output a character selected from the string according to the offset in the index array
   */    
  legacy['KIO'] = function(Pdn,Ps,Pn,Pelem)
  {
    keymanweb.KIO(Pdn,Ps,Pn,Pelem);
  }
  
  legacy['KSETS'] = function(systemId,store,Pelem)
  {
    return keymanweb.KSETS(systemId,store,Pelem);
  }

  /********************************************/
  /*                                          */
  /* Legacy calls to default User Interface   */
  /*                                          */
  /********************************************/
                                          
  /**
   * Function    ShowInterface
   * Scope       Public   
   * Parameters  Px   x-position for KMW window
   *             Py   y-position for KMW window
   * Returns     None 
   * Description Display (default) KMW UI window at specified position, if manual mode
   */    
  legacy['ShowInterface'] = function(Px, Py)
  {
    if(keymanweb['ShowInterface']) keymanweb['ShowInterface'](Px, Py);
  }

  /**
   * Function    HideInterface
   * Scope       Public
   * Parameters  None
   * Returns     None 
   * Description Completely hide (default) KMW UI window 
   */    
  legacy['HideInterface'] = function()
  {
    if(keymanweb['HideInterface']) keymanweb['HideInterface']();
  }
  
  /**
   * Function    IsInterfaceVisible
   * Scope       Public   
   * Parameters  None
   * Returns     True if KMW visible 
   * Description Test if KMW is displayed 
   */    
  legacy['IsInterfaceVisible'] = function()
  {
    if(keymanweb['IsInterfaceVisible']) return keymanweb['IsInterfaceVisible']();
    else return false;
  }
  
  /**
   * Function    SetMode
   * Scope       Public   
   * Parameters  PuiMode  UI mode ('manual' or 'automatic')
   * Returns     None 
   * Description Set UI mode to show user interface when requested
   */    
  legacy['SetMode'] = function(PuiMode)
  {
    if(keymanweb['SetMode']) keymanweb['SetMode'](PuiMode);
  }
  
  /**
   * Function    GetMode
   * Scope       Public   
   * Parameters  None
   * Returns     UI mode  'manual' or 'automatic' 
   * Description Return current UI mode 
   */    
  legacy['GetMode'] = function()
  {
    if(keymanweb['GetMode']) return keymanweb['GetMode'](); 
    else return '';
  } 

  legacy['onshowhelp'] = function()
  {
    var p={};
    osk.doShow(p);
  }

  legacy['onoskhidehelp'] = function()
  {
    var p = {};
    osk.doHide(p);
  }
  
  /**
   * Function      UserInterface
   * Scope         Public
   * Parameters    None
   * Returns       Container DIV for UI
   * Desccription  Lets user interface get or set properties of UI container  
              
    legacy['UserInterface'] = function()
    {
      return keymanweb._DivKeymanWeb;
    }

  
   * Function      UIShim
   * Scope         Public
   * Parameters    None
   * Returns       Container DIV for UI shim
   * Desccription  Lets user interface get or set properties of UI shim element
   *           
  legacy['UIShim'] = function()
  {
    return keymanweb._DivKeymanWebShim;
  }
  

  
   * Function      HelpIcon
   * Scope         Public
   * Parameters    None
   * Returns       OSK Help icon
   * Desccription  Lets user interface get or set properties of help icon
              
  legacy['HelpIcon'] = function()
  {
    return osk._HelpIcon;
  }
  
  
     * Function       SelectKeyboard
   * Scope          Public
   * Parameters     None
   * Returns        Keyboard selection container from UI
   * Description    ***Temporary*** function until SelectKeyboard fully moved into UI code
   *            
  legacy['SelectKeyboard'] = function()
  {
    return keymanweb._SelectKeyboard;
  }   
  */


})(tavultesoft['keymanweb']);
