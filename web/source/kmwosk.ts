/// <reference path="kmwexthtml.ts" />  // Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwstring.ts" />  // Includes KMW string extension declarations.

namespace com.keyman {
  export class OSKKeySpec {
    id: string;
    text?: string;
    sp?: string;
    width: string;
    nextlayer?: string;
    pad?: string;

    constructor(id: string, text?: string, width?: string, sp?: string, nextlayer?: string, pad?: string) {
      this.id = id;
      this.text = text;
      this.width = width ? width : "50";
      this.sp = sp;
      this.nextlayer = nextlayer;
      this.pad = pad;
    }
  }
}

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) {
  /*****************************************/
  /*                                       */
  /*   On-Screen (Visual) Keyboard Code    */
  /*                                       */
  /*****************************************/

  (function() {
    // Declare KeymanWeb and member objects
    var keymanweb=window['keyman'], osk=keymanweb['osk'], util=keymanweb['util'], device=util.device, dbg=keymanweb.debug;
    var kbdInterface=keymanweb['interface'];

    // Define Keyman Developer modifier bit-flags (exposed for use by other modules)
    osk.modifierCodes = {
      "LCTRL":0x0001,
      "RCTRL":0x0002,
      "LALT":0x0004,
      "RALT":0x0008,
      "SHIFT":0x0010,
      "CTRL":0x0020,
      "ALT":0x0040,
      "CAPS":0x0100,
      "NO_CAPS":0x0200,
      "NUM_LOCK":0x0400,
      "NO_NUM_LOCK":0x0800,
      "SCROLL_LOCK":0x1000,
      "NO_SCROLL_LOCK":0x2000,
      "VIRTUAL_KEY":0x4000
    };

    osk.modifierBitmasks = {
      "ALL":0x007F,
      "ALT_GR_SIM": (osk.modifierCodes['LCTRL'] | osk.modifierCodes['LALT']),
      "CHIRAL":0x001F,    // The base bitmask for chiral keyboards.  Includes SHIFT, which is non-chiral.
      "IS_CHIRAL":0x000F, // Used to test if a bitmask uses a chiral modifier.
      "NON_CHIRAL":0x0070 // The default bitmask, for non-chiral keyboards
    };

    osk.stateBitmasks = {
      "ALL":0x3F00,
      "CAPS":0x0300,
      "NUM_LOCK":0x0C00,
      "SCROLL_LOCK":0x3000
    };

    // Define standard keycode numbers (exposed for use by other modules)
    osk.keyCodes={
      "K_BKSP":8,"K_TAB":9,"K_ENTER":13,
      "K_SHIFT":16,"K_CONTROL":17,"K_ALT":18,"K_PAUSE":19,"K_CAPS":20,
      "K_ESC":27,"K_SPACE":32,"K_PGUP":33,
      "K_PGDN":34,"K_END":35,"K_HOME":36,"K_LEFT":37,"K_UP":38,
      "K_RIGHT":39,"K_DOWN":40,"K_SEL":41,"K_PRINT":42,"K_EXEC":43,
      "K_INS":45,"K_DEL":46,"K_HELP":47,"K_0":48,
      "K_1":49,"K_2":50,"K_3":51,"K_4":52,"K_5":53,"K_6":54,"K_7":55,
      "K_8":56,"K_9":57,"K_A":65,"K_B":66,"K_C":67,"K_D":68,"K_E":69,
      "K_F":70,"K_G":71,"K_H":72,"K_I":73,"K_J":74,"K_K":75,"K_L":76,
      "K_M":77,"K_N":78,"K_O":79,"K_P":80,"K_Q":81,"K_R":82,"K_S":83,
      "K_T":84,"K_U":85,"K_V":86,"K_W":87,"K_X":88,"K_Y":89,"K_Z":90,
      "K_NP0":96,"K_NP1":97,"K_NP2":98,
      "K_NP3":99,"K_NP4":100,"K_NP5":101,"K_NP6":102,
      "K_NP7":103,"K_NP8":104,"K_NP9":105,"K_NPSTAR":106,
      "K_NPPLUS":107,"K_SEPARATOR":108,"K_NPMINUS":109,"K_NPDOT":110,
      "K_NPSLASH":111,"K_F1":112,"K_F2":113,"K_F3":114,"K_F4":115,
      "K_F5":116,"K_F6":117,"K_F7":118,"K_F8":119,"K_F9":120,
      "K_F10":121,"K_F11":122,"K_F12":123,"K_NUMLOCK":144,"K_SCROLL":145,
      "K_LSHIFT":160,"K_RSHIFT":161,"K_LCONTROL":162,"K_RCONTROL":163,
      "K_LALT":164,"K_RALT":165,
      "K_COLON":186,"K_EQUAL":187,"K_COMMA":188,"K_HYPHEN":189,
      "K_PERIOD":190,"K_SLASH":191,"K_BKQUOTE":192,
      "K_LBRKT":219,"K_BKSLASH":220,"K_RBRKT":221,
      "K_QUOTE":222,"K_oE2":226,
      "K_LOPT":50001,"K_ROPT":50002,
      "K_NUMERALS":50003,"K_SYMBOLS":50004,"K_CURRENCIES":50005,
      "K_UPPER":50006,"K_LOWER":50007,"K_ALPHA":50008,
      "K_SHIFTED":50009,"K_ALTGR":50010,
      "K_TABBACK":50011,"K_TABFWD":50012
    };

    // Cross-reference with the ids in osk.setButtonClass.
    osk.buttonClasses = {
      'DEFAULT':'0',
      'SHIFT':'1',
      'SHIFT-ON':'2',
      'SPECIAL':'3',
      'SPECIAL-ON':'4',
      'DEADKEY':'8',
      'BLANK':'9',
      'HIDDEN':'10'
    };

    // Defines the PUA code mapping for the various 'special' modifier/control keys on keyboards.
    osk.specialCharacters = {
      '*Shift*':    8,
      '*Enter*':    5,
      '*Tab*':      6,
      '*BkSp*':     4,
      '*Menu*':     11,
      '*Hide*':     10,
      '*Alt*':      25,
      '*Ctrl*':     1,
      '*Caps*':     3,
      '*ABC*':      16,
      '*abc*':      17,
      '*123*':      19,
      '*Symbol*':   21,
      '*Currency*': 20,
      '*Shifted*':  8, // set SHIFTED->9 for filled arrow icon
      '*AltGr*':    2,
      '*TabLeft*':  7,
      '*LAlt*':     0x56,
      '*RAlt*':     0x57,
      '*LCtrl*':    0x58,
      '*RCtrl*':    0x59,
      '*LAltCtrl*':       0x60,
      '*RAltCtrl*':       0x61,
      '*LAltCtrlShift*':  0x62,
      '*RAltCtrlShift*':  0x63,
      '*AltShift*':       0x64,
      '*CtrlShift*':      0x65,
      '*AltCtrlShift*':   0x66,
      '*LAltShift*':      0x67,
      '*RAltShift*':      0x68,
      '*LCtrlShift*':     0x69,
      '*RCtrlShift*':     0x70
    };
   
    osk.modifierSpecials = {
      'leftalt': '*LAlt*',
      'rightalt': '*RAlt*',
      'alt': '*Alt*',
      'leftctrl': '*LCtrl*',
      'rightctrl': '*RCtrl*',
      'ctrl': '*Ctrl*',
      'ctrl-alt': '*AltGr*',
      'leftctrl-leftalt': '*LAltCtrl*',
      'rightctrl-rightalt': '*RAltCtrl*',
      'leftctrl-leftalt-shift': '*LAltCtrlShift*',
      'rightctrl-rightalt-shift': '*RAltCtrlShift*',
      'shift': '*Shift*',
      'shift-alt': '*AltShift*',
      'shift-ctrl': '*CtrlShift*',
      'shift-ctrl-alt': '*AltCtrlShift*',
      'leftalt-shift': '*LAltShift*',
      'rightalt-shift': '*RAltShift*',
      'leftctrl-shift': '*LCtrlShift*',
      'rightctrl-shift': '*RCtrlShift*'
    };

    var codesUS=[['0123456789',';=,-./`','[\\]\''],[')!@#$%^&*(',':+<_>?~','{|}"']];

    var dfltCodes=["K_BKQUOTE","K_1","K_2","K_3","K_4","K_5","K_6","K_7","K_8","K_9","K_0",
      "K_HYPHEN","K_EQUAL","K_*","K_*","K_*","K_Q","K_W","K_E","K_R","K_T",
      "K_Y","K_U","K_I","K_O","K_P","K_LBRKT","K_RBRKT","K_BKSLASH","K_*",
      "K_*","K_*","K_A","K_S","K_D","K_F","K_G","K_H","K_J","K_K","K_L",
      "K_COLON","K_QUOTE","K_*","K_*","K_*","K_*","K_*","K_oE2",
      "K_Z","K_X","K_C","K_V","K_B","K_N","K_M","K_COMMA","K_PERIOD",
      "K_SLASH","K_*","K_*","K_*","K_*","K_*","K_SPACE"];

    var dfltText='`1234567890-=\xA7~~qwertyuiop[]\\~~~asdfghjkl;\'~~~~~?zxcvbnm,./~~~~~ '
                +'~!@#$%^&*()_+\xA7~~QWERTYUIOP{}\\~~~ASDFGHJKL:"~~~~~?ZXCVBNM<>?~~~~~ ';

    osk._Box = null;              // Main DIV for OSK
    osk._DivVKbd = null;
    osk._DivVKbdHelp = null;
    osk._Visible = 0;             // Whether or not actually visible
    osk._Enabled = 1;             // Whether or not enabled by UI
    osk._VShift = [];
    osk._VKeySpans = [];
    osk._VKeyDown = null;
    osk._VKbdContainer = null;
    osk._VOriginalWidth = 1;      // Non-zero default value needed
    osk._BaseLayout = 'us';       // default BaseLayout
    osk._BaseLayoutEuro = {};     // I1299 (not currently exposed, but may need to be e.g. for external users)
    osk._BaseLayoutEuro['se'] = '\u00a71234567890+´~~~QWERTYUIOP\u00c5\u00a8\'~~~ASDFGHJKL\u00d6\u00c4~~~~~<ZXCVBNM,.-~~~~~ ';  // Swedish
    osk._BaseLayoutEuro['uk'] = '`1234567890-=~~~QWERTYUIOP[]#~~~ASDFGHJKL;\'~~~~~\\ZXCVBNM,./~~~~~ '; // UK

    // Tracks the OSK-based state of supported state keys.
    // Using the exact keyCode name from above allows for certain optimizations elsewhere in the code.
    osk._stateKeys = {
      "K_CAPS":false,
      "K_NUMLOCK":false,
      "K_SCROLL":false
    };

    // Additional members (mainly for touch input devices)
    osk.lgTimer = null;           // language switching timer
    osk.lgKey = null;             // language menu key element
    osk.hkKey = null;             // OSK hide key element
    osk.spaceBar = null;          // space bar key element
    osk.lgList = null;            // language menu list
    osk.frameColor = '#ad4a28';   // KeymanWeb standard frame color
    osk.keyPending = null;        // currently depressed key (if any)
    osk.fontFamily = '';          // layout-specified font for keyboard
    osk.fontSize = '1em';         // layout-specified fontsize for keyboard
    osk.layout = null;            // reference to complete layout
    osk.layers = null;            // reference to layout (layers array for this device)
    osk.layerId = 'default';      // currently active OSK layer (if any)
    osk.nextLayer = 'default';    // layer to be activated after pressing key in current layer
    osk.layerIndex = 0;           // currently displayed layer index
    osk.currentKey = '';          // id of currently pressed key (desktop OSK only)
    osk.subkeyDelayTimer = null;  // id for touch-hold delay timer
    osk.popupPending = false;     // Device popup pending flag
    osk.popupVisible = false;     // Device popup displayed
    osk.popupCallout = null;      // OSK popup callout element
    osk.styleSheet = null;        // current OSK style sheet object, if any
    osk.loadRetry = 0;            // counter for delayed loading, if keyboard loading prevents OSK being ready at start
    osk.popupDelay = 500;         // Delay must be less than native touch-hold delay (build 352)
    osk.currentTarget = null;     // Keep track of currently touched key when moving over keyboard
    osk.touchCount = 0;           // Number of active (unreleased) touch points
    osk.touchX = 0;               // First touch point x (to check for sliding off screen)
    osk.deleting = 0;             // Backspace repeat timer

    // Additional members for desktop OSK
    osk.x = 99;                   // last visible offset left
    osk.y = 0;                    // last visible offset top
    osk.width = 1;                // Saved width of OSK (since actual width only available if visible)
    osk.height = 1;               // Saved height of OSK
    osk.rowHeight = 1;            // Current row height in px
    osk.nRows = 1;                // Number of rows in each layer of current layout
    osk.vpScale = 1;              // Current viewport scale factor  (not valid until initialized)
    osk.closeButton = null;       // icon to hide OSK
    osk.resizeIcon = null;        // resizing icon
    osk.resizing = 0;             // resizing flag
    osk.pinImg = null;            // icon to restore OSK to default position
    osk.userPositioned = 0;       // Set to true(<>0) if dragged by user
    osk.dfltX = '';               // Left position set by page code
    osk.dfltY = '';               // Top position set by page code
    osk.noDrag = false;           // allow page to override user OSK dragging
    osk.keytip = null;            // Key preview (phones)
    osk.touchY = 0;               // First y position of touched key

    // Placeholder functions
    osk.addCallout = function(e){};

    osk.shutdown = function() {
      // Remove the OSK's elements from the document, allowing them to be properly cleaned up.
      // Necessary for clean engine testing.
      var _box = osk._Box as HTMLDivElement;
      if(_box.parentElement) {
        _box.parentElement.removeChild(_box);
      }
    }

    /**
     * Function     addEventListener
     * Scope        Public
     * @param       {string}            event     event name
     * @param       {function(Object)}  func      event handler
     * @return      {boolean}
     * Description  Wrapper function to add and identify OSK-specific event handlers
     */
    osk['addEventListener'] = function(event, func)
    {
      return util.addEventListener('osk.'+event, func);
    }

    /**
     * Function     _TitleBarInterior
     * Scope        Private
     * Description  Title bar interior formatting and element event handling
     */
    osk._TitleBarInterior = function()
    {
      var Ldiv = util._CreateElement('DIV');
      var Ls = Ldiv.style;
      Ls.paddingLeft='2px';
      Ls.cursor='move';
      Ls.background='#ad4a28';
      Ls.font='8pt Tahoma,Arial,sans-serif';  //I2186

      // Add container for buttons, handle mousedown event
      var LdivButtons = util._CreateElement('DIV');
      LdivButtons.className = 'kmw-title-bar-actions';
      LdivButtons.onmousedown=osk._CancelMouse;

      // Add close button, handle click and mousedown events
      var Limg = util._CreateElement('DIV');
      Limg.className='kmw-close-button';
      Limg.onmousedown=osk._CancelMouse;
      Limg.onclick=function () {osk._Hide(true);}
      osk.closeButton = Limg;
      LdivButtons.appendChild(Limg);

      /**
       * Move OSK back to default position
       */
      osk.restorePosition = function()
      {
        if(osk._Visible)
        {
          keymanweb.domManager.focusLastActiveElement();  // I2036 - OSK does not unpin to correct location
          osk.loadCookie(); osk.userPositioned=false; osk.saveCookie();
          osk._Show();
          osk.doResizeMove(); //allow the UI to respond to OSK movements
        }
        if(osk.pinImg) osk.pinImg.style.display='none';
        if(window.event) window.event.returnValue=false;
      }

      // Add 'Unpin' button for restoring OSK to default location, handle mousedown and click events
      Limg=osk.pinImg = util._CreateElement('DIV');  //I2186
      Limg.className='kmw-pin-image';
      Limg.title='Pin the On Screen Keyboard to its default location on the active text box';
      Limg.onclick=osk.restorePosition;
      Limg.onmousedown=osk._CancelMouse;
      Limg.style.display='none';

      // Do not use Unpin button on touch screens (OSK location fixed)
      if(!device.touchable) LdivButtons.appendChild(Limg); // I3363 (Build 301)

      // Attach button container to title bar
      Ldiv.appendChild(LdivButtons);

      // Add title bar caption
      Limg=keymanweb._TitleElement=util._CreateElement('SPAN');  // I1972
      Limg.className='kmw-title-bar-caption';
      Limg.innerHTML='KeymanWeb';
      Ldiv.appendChild(Limg);

      return Ldiv;
    }

    // End of TitleBarInterior

    /**
     * Function     enabled
     * Scope        Public
     * @return      {boolean|number}    True if KMW OSK enabled
     * Description  Test if KMW OSK is enabled
     */
    osk['isEnabled'] = osk.isEnabled = function()
    {
      return osk._Enabled;
    }

    /**
     * Function     isVisible
     * Scope        Public
     * @return      {boolean|number}    True if KMW OSK visible
     * Description  Test if KMW OSK is actually visible
     * Note that this will usually return false after any UI event that results in (temporary) loss of input focus
     */
    osk['isVisible'] = osk.isVisible = function()
    {
      return osk._Visible;
    }

    /**
     * Function     getRect //TODO:  This is probably not correct, anyway!!!!!
     * Scope        Public
     * @return      {Object.<string,number>}   Array object with position and size of OSK container
     * Description  Get rectangle containing KMW Virtual Keyboard
     */
    osk['getRect'] = osk.getRect = function() {		// I2405
      var p: any;
      p = {};

      if(osk._DivVKbd) {
        p['left'] = p.left = util._GetAbsoluteX(osk._DivVKbd);
        p['top']  = p.top  = util._GetAbsoluteY(osk._DivVKbd);
        p['width']  = p.width  = util._GetAbsoluteX(osk._DivVKbdHelp) - util._GetAbsoluteX(osk._DivVKbd) + osk._DivVKbdHelp.offsetWidth;
        p['height'] = p.height = util._GetAbsoluteY(osk._DivVKbdHelp) - util._GetAbsoluteY(osk._DivVKbd) + osk._DivVKbdHelp.offsetHeight;
      } else {
        p['left'] = p.left = util._GetAbsoluteX(osk._Box);
        p['top']  = p.top  = util._GetAbsoluteY(osk._Box);
        p['width']  = p.width  = util._GetAbsoluteX(osk._Box) + osk._Box.offsetWidth;
        p['height'] = p.height = util._GetAbsoluteY(osk._Box) + osk._Box.offsetHeight;
      }
      return p;
    }

    /**
     * Allow the UI or page to set the position and size of the OSK
     * and (optionally) override user repositioning or sizing
     *
     * @param       {Object.<string,number>}   p  Array object with position and size of OSK container
    **/
    osk['setRect'] = osk.setRect = function(p)
    {
      var q={};

      if(osk._Box == null || device.formFactor != 'desktop') return;

      var b=osk._Box,bs=b.style;
      if('left' in p)
      {
        bs.left=(p['left']-util._GetAbsoluteX(b)+b.offsetLeft)+'px';
        osk.dfltX=bs.left;
      }

      if('top' in p)
      {
        bs.top=(p['top']-util._GetAbsoluteY(b)+b.offsetTop)+'px';
        osk.dfltY=bs.top;
      }

      //Do not allow user resizing for non-standard keyboards (e.g. EuroLatin)
      if(osk._DivVKbd != null)
      {
        var d=osk._DivVKbd,ds=d.style;

        // Set width, but limit to reasonable value
        if('width' in p)
        {
          var w=(p['width']-(b.offsetWidth-d.offsetWidth));
          if(w < 0.2*screen.width) w=0.2*screen.width;
          if(w > 0.9*screen.width) w=0.9*screen.width;
          ds.width=w+'px'; osk.width=w;
        }

        // Set height, but limit to reasonable value
        // This sets the default font size for the OSK in px, but that
        // can be modified at the key text level by setting
        // the font size in em in the kmw-key-text class
        if('height' in p)
        {
          var h=(p['height']-(b.offsetHeight-d.offsetHeight));
          if(h < 0.1*screen.height) h=0.1*screen.height;
          if(h > 0.5*screen.height) h=0.5*screen.height;
          ds.height=h+'px'; ds.fontSize=(h/8)+'px'; osk.height=h;
        }

        // Fix or release user resizing
        if('nosize' in p)
          if(osk.resizeIcon) osk.resizeIcon.style.display=(p['nosize'] ? 'none' : 'block');

      }
      // Fix or release user dragging
      if('nomove' in p)
      {
        osk.noDrag=p['nomove'];
        if(osk.pinImg) osk.pinImg.style.display=(p['nomove'] || !osk.userPositioned) ? 'none' : 'block';
      }
      // Save the user-defined OSK size
      osk.saveCookie();
    }


    /**
     * Get position of OSK window
     *
     * @return      {Object.<string,number>}     Array object with OSK window position
    **/
    osk.getPos = function() {
      var Lkbd=osk._Box, p={
        left: osk._Visible ? Lkbd.offsetLeft : osk.x,
        top:osk._Visible ? Lkbd.offsetTop : osk.y
      };

      return p;
    }

    /**
     * Function     setPos
     * Scope        Private
     * @param       {Object.<string,number>}    p     Array object with OSK left, top
     * Description  Set position of OSK window, but limit to screen, and ignore if  a touch input device
     */
    osk['setPos'] = osk.setPos = function(p)
    {
      if(typeof(osk._Box) == 'undefined' || device.touchable) return; // I3363 (Build 301)
      if(osk.userPositioned)
      {
        var Px=p['left'],Py=p['top'];
        if(typeof(Px) != 'undefined')
        {
          if(Px < -0.8*osk._Box.offsetWidth) Px = -0.8*osk._Box.offsetWidth;
          if(osk.userPositioned) {osk._Box.style.left=Px+'px'; osk.x = Px;}
        }
        // May not be needed - vertical positioning is handled differently and defaults to input field if off screen
        if(typeof(Py) != 'undefined')
        {
          if(Py < 0) Py = 0;
          if(osk.userPositioned) {osk._Box.style.top=Py+'px'; osk.y = Py;}
        }
      }

      if(osk.pinImg)
        osk.pinImg.style.display=(osk.userPositioned ? 'block' : 'none');
    }

    /**
     * Function     _VKeyGetTarget
     * Scope        Private
     * @param       {Object}    e     OSK event
     * @return      {Object}          Target element for key in OSK
     * Description  Identify the OSK key clicked
     */
    osk._VKeyGetTarget = function(e) {
      var Ltarg;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      Ltarg = util.eventTarget(e);
      if (Ltarg == null) {
        return null;
      }
      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode;
      }
      if (Ltarg.tagName == 'SPAN') {
        Ltarg = Ltarg.parentNode;
      }
      return Ltarg;
    }

    /**
     *  Add or remove a class from a keyboard key (when touched or clicked)
     *  or add a key preview for phone devices
     *
     *  @param    {Object}    key   key affected
     *  @param    {boolean}   on    add or remove highlighting
     **/
    osk.highlightKey = function(key,on)
    {
      // Do not change element class unless a key
      if(!key || (key.className == '') || (key.className.indexOf('kmw-key-row') >= 0)) return;

      var classes=key.className, cs = ' kmw-key-touched';

      // For phones, use key preview rather than highlighting the key,
      // except for space, bksp, enter, shift and popup keys
      var usePreview = ((osk.keytip != null)
        && (classes.indexOf('kmw-key-shift') < 0)
        && (classes.indexOf('kmw-spacebar') < 0)
        && (key.id.indexOf('popup') < 0 ));

      if(usePreview)
      {
        osk.showKeyTip(key,on);
      }
      else
      {
        if(on && classes.indexOf(cs) < 0)
        {
          key.className=classes+cs;
          osk.showKeyTip(null,false);     // Moved here by Serkan
        }
        else
        {
          key.className=classes.replace(cs,'');
        }
      }
    }

    /**
     * Display touch-hold array of 'sub-keys' above the currently touched key
     * @param       {Object}    e      primary key element
     */
    osk.showSubKeys = function(e) {
      // Do not show subkeys if key already released
      if(osk.keyPending == null) {
        return;
      }

      // Create holder DIV for subkey array, and set styles.
      // A subkey array for Shift will only appear if extra layers exist

      // The holder is position:fixed, but the keys do not need to be, as no scrolling
      // is possible while the array is visible.  So it is simplest to let the keys have
      // position:static and display:inline-block
      var subKeys=document.createElement('DIV'),i,sk,
        t,ts,t1,ts1,kDiv,ks,btn,bs;

      var tKey = osk.getDefaultKeyObject();

      subKeys.id='kmw-popup-keys';
      osk.popupBaseKey = e;

      // Does the popup array include the base key?   *** condition for phone only ***
      if(device.formFactor == 'phone') {
        osk.prependBaseKey(e);
      }
      var idx = e.id.split('-'), baseId = idx[idx.length-1];

      // If not, insert at start
      if(device.formFactor == 'phone' && e.subKeys[0].id != baseId) {
        var eCopy={'id':baseId,'layer':''};
        if(idx.length > 1) {
          eCopy['layer'] = idx[0];
        }
        for(i=0; i<e.childNodes.length; i++) {
          if(osk.hasClass(e.childNodes[i],'kmw-key-text')) {
            break;
          }
        }
        if(i < e.childNodes.length) {
          eCopy['text'] = e.childNodes[i].textContent;
        }
        e.subKeys.splice(0,0,eCopy);
      }

      // Must set position dynamically, not in CSS
      var ss=subKeys.style;
      ss.bottom=(parseInt(e.style.bottom,10)+parseInt(e.style.height,10)+4)+'px';

      // Set key font according to layout, or defaulting to OSK font
      // (copied, not inherited, since OSK is not a parent of popup keys)
      ss.fontFamily=osk.fontFamily;

      // Copy the font size from the parent key, allowing for style inheritance
      ss.fontSize=util.getStyleValue(e,'font-size');
      ss.visibility='hidden';

      var nKeys=e.subKeys.length,nRow,nRows,nCols;
      nRows=Math.min(Math.ceil(nKeys/9),2);
      nCols=Math.ceil(nKeys/nRows);
      if(nRows > 1) {
        ss.width=(nCols*e.offsetWidth+nCols*5)+'px';
      }

      // Add nested button elements for each sub-key
      for(i=0; i<nKeys; i++) {
        sk=e.subKeys[i];
        kDiv=document.createElement('DIV');

        for(var tp in tKey) {
          if(typeof sk[tp] != 'string') {
            sk[tp]=tKey[tp];
          }
        }

        kDiv.className='kmw-key-square-ex';
        kDiv.keyId=sk['id'];
        ks=kDiv.style;
        nRow=Math.floor(i/nCols);
        if(nRows > 1 && nRow > 0) {
          ks.marginTop='5px';
        }

        if(typeof sk['width'] != 'undefined') {
          kDiv.width=ks.width=(parseInt(sk['width'],10)*e.offsetWidth/100)+'px';
        } else {
          kDiv.width=ks.width=e.offsetWidth+'px';
        }
        ks.height=e.offsetHeight+'px';

        btn=document.createElement('DIV');
        osk.setButtonClass(sk,btn);

        // Create (temporarily) unique ID by prefixing 'popup-' to actual key ID
        if(typeof(sk['layer']) == 'string' && sk['layer'] != '') {
          btn.id='popup-'+sk['layer']+'-'+sk['id'];
        } else {
          btn.id='popup-' + osk.layerId + '-'+sk['id'];
        }

        btn.key = sk;

        // Must set button size (in px) dynamically, not from CSS
        bs=btn.style; bs.height=ks.height; bs.width=ks.width;

        // Must set position explicitly, at least for Android
        bs.position='absolute';
        t=util._CreateElement('SPAN');
        t.className='kmw-key-text';
        if(sk['text'] == null || sk['text'] == '') {
          t.innerHTML='\xa0';
          if(typeof sk['id'] == 'string') {
            if(/^U_[0-9A-F]{4}$/i.test(sk['id'])) {
              t.innerHTML=String.fromCharCode(parseInt(sk['id'].substr(2),16));
            }
          }
        } else {
          t.innerHTML=sk['text'];
        }

        // Override the font name and size if set in the layout
        ts=t.style;
        ts.fontSize=osk.fontSize;     //Build 344, KMEW-90
        if(typeof sk['font'] == 'string' && sk['font'] != '') {
          ts.fontFamily=sk['font'];
        }
        if(typeof sk['fontsize'] == 'string' && sk['fontsize'] != 0) {
          ts.fontSize=sk['fontsize'];
        }

        btn.appendChild(t);
        kDiv.appendChild(btn);
        subKeys.appendChild(kDiv);
      }

      // Clear key preview if any
      osk.showKeyTip(null,false);

      // Otherwise append the touch-hold (subkey) array to the OSK
      osk._Box.appendChild(subKeys);

      // And correct its position with respect to that element
      ss=subKeys.style;
      var x=util._GetAbsoluteX(e)+0.5*(e.offsetWidth-subKeys.offsetWidth), y,
        xMax=(util.landscapeView()?screen.height:screen.width)-subKeys.offsetWidth;

      if(x > xMax) {
        x=xMax;
      }
      if(x < 0) {
        x=0;
      }
      ss.left=x+'px';

      // Add the callout
      osk.popupCallout = osk.addCallout(e);

      // Make the popup keys visible
      ss.visibility='visible';

      // And add a filter to fade main keyboard
      subKeys.shim = document.createElement('DIV');
      subKeys.shim.id = 'kmw-popup-shim';
      osk._Box.appendChild(subKeys.shim);

      // Highlight the duplicated base key (if a phone)
      if(device.formFactor == 'phone') {
        var bk = subKeys.childNodes[0].firstChild;
        osk.keyPending = bk;
        osk.highlightKey(bk,true);//bk.className = bk.className+' kmw-key-touched';
      }
    }

    /**
     * Prepend the base key to the touch-hold key array (for phones)
     *
     * @param {Object}  e   base key object
     */
    osk.prependBaseKey = function(e)
    {
      if(e && typeof(e.id) != 'undefined')
      {
        //TODO: refactor this, it's pretty messy...
        var i, 
          idx = e.id.split('-'), 
          baseId = idx[idx.length-1], 
          layer = e['key'] && e['key']['layer'] ? e['key']['layer'] : (idx.length > 1 ? idx[0] : ''),
          sp = e['key'] && e['key']['sp'],
          nextlayer = e['key'] && e['key']['nextlayer'] ? e['key']['nextlayer'] : null;
        if(typeof e.subKeys != 'undefined' && e.subKeys.length > 0 && (e.subKeys[0].id != baseId || e.subKeys[0].layer != layer))
        {
          var eCopy={'id':baseId,'layer':'','key':undefined};
          if(layer != '') {
            eCopy['layer'] = layer;
          }
          if(sp) {
            eCopy['sp'] = sp;
          }
          if(nextlayer) {
            eCopy['nextlayer'] = nextlayer;
          }
          for(i = 0; i < e.childNodes.length; i++) {
            if(osk.hasClass(e.childNodes[i],'kmw-key-text')) break;
          }
          if(i < e.childNodes.length) eCopy['text'] = e.childNodes[i].textContent;
          e.subKeys.splice(0,0,eCopy);
        }
      }
    }

    /**
     * @summary Look up a custom virtual key code in the virtual key code dictionary KVKD.  On first run, will build the dictionary.
     *
     * `VKDictionary` is constructed from the keyboard's `KVKD` member. This list is constructed 
     * at compile-time and is a list of 'additional' virtual key codes, starting at 256 (i.e. 
     * outside the range of standard virtual key codes). These additional codes are both 
     * `[T_xxx]` and `[U_xxxx]` custom key codes from the Keyman keyboard language. However, 
     * `[U_xxxx]` keys only generate an entry in `KVKD` if there is a corresponding rule that 
     * is associated with them in the keyboard rules. If the `[U_xxxx]` key code is only 
     * referenced as the id of a key in the touch layout, then it does not get an entry in 
     * the `KVKD` property.
     *
     * @private
     * @param       {string}      keyName   custom virtual key code to lookup in the dictionary
     * @return      {number}                key code > 255 on success, or 0 if not found
     */
    osk.getVKDictionaryCode = function(keyName) {
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;
      if(!activeKeyboard['VKDictionary']) {
        var a=[];
        if(typeof activeKeyboard['KVKD'] == 'string') {
          // Build the VK dictionary
          // TODO: Move the dictionary build into the compiler -- so compiler generates code such as following.  
          // Makes the VKDictionary member unnecessary.
          //       this.KVKD={"K_ABC":256,"K_DEF":257,...};
          var s=activeKeyboard['KVKD'].split(' ');
          for(var i=0; i<s.length; i++) {
            a[s[i].toUpperCase()]=i+256; // We force upper-case since virtual keys should be case-insensitive.
          }
        }
        activeKeyboard['VKDictionary']=a;
      }

      var res=activeKeyboard['VKDictionary'][keyName.toUpperCase()];
      return res ? res : 0;
    }
    /**
     * Select the next keyboard layer for layer switching keys
     * The next layer will be determined from the key name unless otherwise specifed
     *
     *  @param  {string}                    keyName     key identifier
     *  @param  {number|string|undefined}   nextLayerIn optional next layer identifier
     *  @return {boolean}                               return true if keyboard layer changed
     */
    osk.selectLayer = function(keyName,nextLayerIn)
    {
      var nextLayer = arguments.length < 2 ? null : nextLayerIn;
      var isChiral = keymanweb.keyboardManager.isChiral();

      // Layer must be identified by name, not number (27/08/2015)
      if(typeof nextLayer == 'number')  nextLayer = osk.getLayerId(nextLayer * 0x10);

      // Identify next layer, if required by key
      if(!nextLayer) switch(keyName)
      {
        case 'K_LSHIFT':
        case 'K_RSHIFT':
        case 'K_SHIFT':
          nextLayer = 'shift'; break;
        case 'K_LCONTROL':
        case 'K_LCTRL':
          if(isChiral) {
            nextLayer = 'leftctrl';
            break;
          }
        case 'K_RCONTROL':
        case 'K_RCTRL':
          if(isChiral) {
            nextLayer = 'rightctrl';
            break;
          }
        case 'K_CTRL':
          nextLayer = 'ctrl'; break;
        case 'K_LMENU':
        case 'K_LALT':
          if(isChiral) {
            nextLayer = 'leftalt';
            break;
          }
        case 'K_RMENU':
        case 'K_RALT':
          if(isChiral) {
            nextLayer = 'rightalt';
            break;
          }
        case 'K_ALT':
          nextLayer = 'alt'; break;
        case 'K_ALTGR':
          if(isChiral) {
            nextLayer = 'leftctrl-rightalt';
          } else {
            nextLayer = 'ctrl-alt';
          }
          break;
        case 'K_CURRENCIES':
        case 'K_NUMERALS':
        case 'K_SHIFTED':
        case 'K_UPPER':
        case 'K_LOWER':
        case 'K_SYMBOLS':
          nextLayer = 'default'; break;
      }

      if(!nextLayer) return false;

      // Do not change layer unless needed (27/08/2015)
      if(nextLayer == osk.layerId && device.formFactor != 'desktop') return false;

      // Change layer and refresh OSK
      osk.updateLayer(nextLayer);
      osk._Show();

      return true;
    }

    /**
     * Get the default key string. If keyName is U_xxxxxx, use that Unicode codepoint.
     * Otherwise, lookup the  virtual key code (physical keyboard mapping)
     *
     * @param   {string}  keyName Name of the key
     * @param   {number}  n
     * @param   {number}  keyShiftState
     * @param   {boolean} usingOSK
     * @param   {Object=} Lelem
     * @return  {string}
     */
    osk.defaultKeyOutput = function(keyName,n,keyShiftState,usingOSK,Lelem) {
      var ch = '', checkCodes = false;
      var touchAlias = (Lelem && typeof(Lelem.base) != 'undefined');
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState == 0) {
        checkCodes = true;
      } else if (keyShiftState == osk.modifierCodes['SHIFT']) {
        checkCodes = true; 
        keyShiftState = 1; // It's used as an index.
      } else {
        console.warn("KMW only defines default key output for the 'default' and 'shift' layers!");
      }

      // If this was triggered by the OSK -or- if it was triggered within a touch-aliased DIV element.
      if(touchAlias || usingOSK) {
        var code = osk.keyCodes[keyName];
        if(!code) {
          code = n;
        }

        switch(code) {
          case osk.keyCodes['K_BKSP']:  //Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
            kbdInterface.defaultBackspace();
            break;
          case osk.keyCodes['K_TAB']:
            keymanweb.domManager.moveToNext(keyShiftState);
            break;
          case osk.keyCodes['K_TABBACK']:
            keymanweb.domManager.moveToNext(true);
            break;
          case osk.keyCodes['K_TABFWD']:
            keymanweb.domManager.moveToNext(false);
            break;
          case osk.keyCodes['K_ENTER']:
            // Insert new line in text area fields
            if(Lelem.nodeName == 'TEXTAREA' || (typeof Lelem.base != 'undefined' && Lelem.base.nodeName == 'TEXTAREA')) {
              return '\n';
            // Or move to next field from TEXT fields
            } else if(usingOSK) {
              if(Lelem.nodeName == 'INPUT' && (Lelem.type == 'search' || Lelem.type == 'submit')) {
                Lelem.form.submit();
              } else if(typeof(Lelem.base) != 'undefined' && (Lelem.base.type == 'search' || Lelem.base.type == 'submit')) {
                Lelem.base.disabled=false;
                Lelem.base.form.submit();
              } else {
                keymanweb.domManager.moveToNext(false);
              }
            }
            break;
          case osk.keyCodes['K_SPACE']:
            return ' ';
          // break;
          //
          // // Problem:  clusters, and doing them right.
          // // The commented-out code below should be a decent starting point, but clusters make it complex.
          //
          // case osk.keyCodes['K_LEFT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos - 1 >= 0 ? caretPos - 1 : 0);
          //   }
          //   break;
          // case osk.keyCodes['K_RIGHT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos + 1);
          //   }
          //   if(code == osk.keyCodes['K_RIGHT']) {
          //     break;
          //   }
          // // Should we include this?  It could be tricky to do correctly...
          // case osk.keyCodes['K_DEL']:
          //   // Move caret right one unit, then backspace.
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos + 1);
          //     if(caretPos == keymanweb.getTextCaret(Lelem)) {
          //       // Failed to move right - there's nothing to delete.
          //       break;
          //     }
          //     kbdInterface.defaultBackspace();
          //   }
        }
      }

      // TODO:  Refactor the overloading of the 'n' parameter here into separate methods.

      // Test for fall back to U_xxxxxx key id
      // For this first test, we ignore the keyCode and use the keyName
      if((keyName.substr(0,2) == 'U_')) {
        var codePoint = parseInt(keyName.substr(2,6), 16);
        if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
          // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
          // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
          console.log("Suppressing Unicode control code: U_00" + codePoint.toString(16));
          return ch;
        } else {
          // String.fromCharCode() is inadequate to handle the entire range of Unicode
          // Someday after upgrading to ES2015, can use String.fromCodePoint()
          ch=String.kmwFromCharCode(codePoint);
        }
        // Hereafter, we refer to keyCodes.
      } else if(checkCodes) { // keyShiftState can only be '1' or '2'.
        try {
          if(n >= osk.keyCodes['K_0'] && n <= osk.keyCodes['K_9']) { // The number keys.
            ch = codesUS[keyShiftState][0][n-osk.keyCodes['K_0']];
          } else if(n >=osk.keyCodes['K_A'] && n <= osk.keyCodes['K_Z']) { // The base letter keys
            ch = String.fromCharCode(n+(keyShiftState?0:32));  // 32 is the offset from uppercase to lowercase.
          } else if(n >= osk.keyCodes['K_COLON'] && n <= osk.keyCodes['K_BKQUOTE']) {
            ch = codesUS[keyShiftState][1][n-osk.keyCodes['K_COLON']];
          } else if(n >= osk.keyCodes['K_LBRKT'] && n <= osk.keyCodes['K_QUOTE']) {
            ch = codesUS[keyShiftState][2][n-osk.keyCodes['K_LBRKT']];
          }
        } catch (e) {
          console.error("Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default'));
        }
      }
      return ch;
    }

    /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Note that the test-case oriented 'recorder' stubs this method to facilitate OSK-based input
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     * 
     * @param       {Object}      e      element touched (or clicked)
     */
    osk.clickKey = function(e) {
      var Lelem = keymanweb.domManager.getLastActiveElement(), Ls, Le, Lkc;

      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;

      // Each button id is of the form <layer>-<keyCode>, e.g. 'shift-ctrl-K_Q' or 'popup-shift-K_501', etc.
      var t=e.id.split('-');
      if(t.length < 2) return true; //shouldn't happen, but...

      // Remove popup prefix before processing keystroke (KMEW-93)
      if(t[0] == 'popup') t.splice(0,1);

      if(Lelem != null) {
        // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
        var layer=t[0], keyName=t[t.length-1], keyShiftState=osk.getModifierState(osk.layerId),
          nextLayer = keyShiftState;

        // Make sure to get the full current layer, since layers are now kebab-case.
        for(var i=1; i < t.length-1; i++) {
          layer = layer + "-" + t[i];
        }

        if(typeof(e.key) != 'undefined') nextLayer=e.key['nextlayer'];
        keymanweb.domManager.initActiveElement(Lelem);

        // Exclude menu and OSK hide keys from normal click processing
        if(keyName == 'K_LOPT' || keyName == 'K_ROPT') {
          osk.optionKey(e,keyName,true);
          return true;
        }

        // Turn off key highlighting (or preview)
        osk.highlightKey(e,false);

        // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
        if(device.formFactor == 'desktop') {
          if(osk.selectLayer(keyName, nextLayer)){
            return true;
          }
        }

        // Prevent any output from 'ghost' (unmapped) keys
        if(keyName != 'K_SPACE')
        {
          var keyText=e.childNodes[0].innerHTML;
          //// if(keyText == '' || keyText == '&nbsp;') return true; --> why?
        }

        Ls=Lelem._KeymanWebSelectionStart;
        Le=Lelem._KeymanWebSelectionEnd;
        keymanweb.uiManager.setActivatingUI(true);
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 100;
        keymanweb.domManager.focusLastActiveElement();
        if(keymanweb.domManager._IsMozillaEditableIframe(Lelem,0)) Lelem = Lelem.documentElement;
        Lelem._KeymanWebSelectionStart=Ls;
        Lelem._KeymanWebSelectionEnd=Le;
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 0;
        // ...end I3363 (Build 301)
        keymanweb._CachedSelectionStart = null; // I3319
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        kbdInterface._DeadkeyDeleteMatched();      // Delete any matched deadkeys before continuing
        //kbdInterface._DeadkeyResetMatched();       // I3318   (Not needed if deleted first?)



        // First check the virtual key, and process shift, control, alt or function keys
        Lkc = {Ltarg:Lelem,Lmodifiers:0,Lstates:0,Lcode:osk.keyCodes[keyName],LisVirtualKey:true};

        // Set the flags for the state keys.
        Lkc.Lstates |= osk._stateKeys['K_CAPS']    ? osk.modifierCodes['CAPS'] : osk.modifierCodes['NO_CAPS'];
        Lkc.Lstates |= osk._stateKeys['K_NUMLOCK'] ? osk.modifierCodes['NUM_LOCK'] : osk.modifierCodes['NO_NUM_LOCK'];
        Lkc.Lstates |= osk._stateKeys['K_SCROLL']  ? osk.modifierCodes['SCROLL_LOCK'] : osk.modifierCodes['NO_SCROLL_LOCK'];

        // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
        if(keyName.substr(0,2) == 'U_') Lkc.LisVirtualKey=false;

        // Get code for non-physical keys (T_KOKAI, U_05AB etc)
        if(typeof Lkc.Lcode == 'undefined')
        {
          Lkc.Lcode = osk.getVKDictionaryCode(keyName);// Updated for Build 347
          if(!Lkc.Lcode) 
          {
            // Special case for U_xxxx keys. This vk code will never be used
            // in a keyboard, so we use this to ensure that keystroke processing
            // occurs for the key.
            Lkc.Lcode = 1; 
          }
        }

        // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
        var lx=(typeof e.key == 'undefined' ? null : e.key['layer']);
        if(lx == null)
          keyShiftState=osk.getModifierState(layer);
        else
          keyShiftState=osk.getModifierState(lx);

        // Define modifiers value for sending to keyboard mapping function
        Lkc.Lmodifiers = keyShiftState;

        // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
        if((Lkc.Lmodifiers & osk.modifierBitmasks['ALT_GR_SIM']) == osk.modifierBitmasks['ALT_GR_SIM'] && osk.emulatesAltGr()) {
          Lkc.Lmodifiers &= ~osk.modifierBitmasks['ALT_GR_SIM'];
          Lkc.Lmodifiers |= osk.modifierCodes['RALT'];
        }

        // Include *limited* support for mnemonic keyboards (Sept 2012)
        // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
        if(activeKeyboard && activeKeyboard['KM'] && !(activeKeyboard['KVKL'] && device.formFactor != 'desktop')) {
          if(Lkc.Lcode != osk.keyCodes['K_SPACE']) { // exception required, March 2013
            Lkc.vkCode = Lkc.Lcode;
            // So long as the key name isn't prefixed with 'U_', we'll get a default mapping based on the Lcode value.
            // We need to determine the mnemonic base character - for example, SHIFT + K_PERIOD needs to map to '>'.
            var mappedChar: string = osk.defaultKeyOutput('K_xxxx', Lkc.Lcode, (layer.indexOf('shift') != -1 ? 0x10 : 0), false, null);
            if(mappedChar) {
              Lkc.Lcode = mappedChar.charCodeAt(0);
            } // No 'else' - avoid remapping control + modifier keys!

            if(osk._stateKeys['K_CAPS']) {
              if((Lkc.Lcode >= 65 && Lkc.Lcode <= 90) /* 'A' - 'Z' */ || (Lkc.Lcode >= 97 && Lkc.Lcode <= 122) /* 'a' - 'z' */) {
                Lkc.Lmodifiers ^= 0x10; // Flip the 'shift' bit.
                Lkc.Lcode ^= 0x20; // Flips the 'upper' vs 'lower' bit for the base 'a'-'z' ASCII alphabetics.
              }
            }
          }
        } else {
          Lkc.vkCode=Lkc.Lcode;
        }

        // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
        if(typeof activeKeyboard['KM'] == 'undefined')
        {
          Lkc.Lcode=keymanweb.keyMapManager._USKeyCodeToCharCode(Lkc);
          Lkc.LisVirtualKey=false;
        }

        // Pass this key code and state to the keyboard program
        if(!activeKeyboard || (Lkc.Lcode != 0 && !kbdInterface.processKeystroke(util.device, Lelem, Lkc)))
        {
          // Restore the virtual key code if a mnemonic keyboard is being used
          Lkc.Lcode=Lkc.vkCode;

          // Handle unmapped keys, including special keys
          switch(keyName)
          {
            case 'K_CAPS':
            case 'K_NUMLOCK':
            case 'K_SCROLL':
              osk._stateKeys[keyName] = ! osk._stateKeys[keyName];
              osk._Show();
              break;
            default:
              // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
              var ch = osk.defaultKeyOutput(keyName,Lkc.Lcode,keyShiftState,true,Lelem);
              if(ch) {
                kbdInterface.output(0, Lelem, ch);
              }
          }
        }

        // Test if this key has a non-default next layer
        if(typeof e.key != 'undefined' && e.key['nextlayer'] !== null) osk.nextLayer=e.key['nextlayer'];

        // Swap layer as appropriate.
        osk.selectLayer(keyName, nextLayer);

        /* I732 END - 13/03/2007 MCD: End Positional Layout support in OSK */
        Lelem._KeymanWebSelectionStart=null;
        Lelem._KeymanWebSelectionEnd=null;
      }
      keymanweb.uiManager.setActivatingUI(false);	// I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
      return true;
    }

    /**
     * Function     getLayerId
     * Scope        Private
     * @param       {number}      m     shift modifier code
     * @return      {string}            layer string from shift modifier code (desktop keyboards)
     * Description  Get name of layer from code, where the modifer order is determined by ascending bit-flag value.
     */
    osk.getLayerId = function(m)
    {
      var s='';
      if(m == 0) {
        return 'default';
      } else {
        if(m & osk.modifierCodes['LCTRL']) {
          s = (s.length > 0 ? s + '-' : '') + 'leftctrl';
        }
        if(m & osk.modifierCodes['RCTRL']) {
          s = (s.length > 0 ? s + '-' : '') + 'rightctrl';
        }
        if(m & osk.modifierCodes['LALT']) {
          s = (s.length > 0 ? s + '-' : '') + 'leftalt';
        }
        if(m & osk.modifierCodes['RALT']) {
          s = (s.length > 0 ? s + '-' : '') + 'rightalt';
        }
        if(m & osk.modifierCodes['SHIFT']) {
          s = (s.length > 0 ? s + '-' : '') + 'shift';
        }
        if(m & osk.modifierCodes['CTRL']) {
          s = (s.length > 0 ? s + '-' : '') + 'ctrl';
        }
        if(m & osk.modifierCodes['ALT']) {
          s = (s.length > 0 ? s + '-' : '') + 'alt';
        }
        return s;
      }
    }

    /**
     * Get modifier key state from layer id
     *
     * @param       {string}      layerId       layer id (e.g. ctrlshift)
     * @return      {number}                    modifier key state (desktop keyboards)
     */
    osk.getModifierState = function(layerId)
    {
        var modifier=0;
        if(layerId.indexOf('shift') >= 0) {
          modifier |= osk.modifierCodes['SHIFT'];
        }

        // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
        var ctrlMatched=false;
        if(layerId.indexOf('leftctrl') >= 0) {
          modifier |= osk.modifierCodes['LCTRL'];
          ctrlMatched=true;
        } 
        if(layerId.indexOf('rightctrl') >= 0) {
          modifier |= osk.modifierCodes['RCTRL'];
          ctrlMatched=true;
        } 
        if(layerId.indexOf('ctrl')  >= 0 && !ctrlMatched) {
          modifier |= osk.modifierCodes['CTRL'];
        }

        var altMatched=false;
        if(layerId.indexOf('leftalt') >= 0) {
          modifier |= osk.modifierCodes['LALT'];
          altMatched=true;
        } 
        if(layerId.indexOf('rightalt') >= 0) {
          modifier |= osk.modifierCodes['RALT'];
          altMatched=true;
        } 
        if(layerId.indexOf('alt')  >= 0 && !altMatched) {
          modifier |= osk.modifierCodes['ALT'];
        }

        return modifier;
    }

    /**
     * Sets the new layer id, allowing for toggling shift/ctrl/alt while preserving the remainder
     * of the modifiers represented by the current layer id (where applicable)
     *
     * @param       {string}      id      layer id (e.g. ctrlshift)
     */
    osk.updateLayer = function(id)
    {
      var s=osk.layerId,idx=id;
      var i;

      // Need to test if target layer is a standard layer (based on the plain 'default')
      var replacements= ['leftctrl', 'rightctrl', 'ctrl', 'leftalt', 'rightalt', 'alt', 'shift'];

      for(i=0; i < replacements.length; i++) {
        // Don't forget to remove the kebab-case hyphens!
        idx=idx.replace(replacements[i] + '-', '');
        idx=idx.replace(replacements[i],'');
      }

      // If we are presently on the default layer, drop the 'default' and go straight to the shifted mode.
      // If on a common symbolic layer, drop out of symbolic mode and go straight to the shifted mode.
      if(osk.layerId == 'default' || osk.layerId == 'numeric' || osk.layerId == 'symbol' || osk.layerId == 'currency' || idx != '') {
        s = id;
      }
      // Otherwise, we are based upon the a layer that accepts modifier variations.
      // Modify the layer according to the current state and key pressed.
      //
      // TODO:  Consider:  should this ever be allowed for a base layer other than 'default'?  If not,
      // if(idx == '') with accompanying if-else structural shift would be a far better test here.
      else
      {
        // Save our current modifier state.
        var modifier=osk.getModifierState(s);

        // Strip down to the base modifiable layer.
        for(i=0; i < replacements.length; i++) {
          // Don't forget to remove the kebab-case hyphens!
          s=s.replace(replacements[i] + '-', '');
          s=s.replace(replacements[i],'');
        }

        // Toggle the modifier represented by our input argument.
        switch(id)
        {
          case 'shift':
            modifier ^= osk.modifierCodes['SHIFT'];
            break;
          case 'leftctrl':
            modifier ^= osk.modifierCodes['LCTRL'];
            break;
          case 'rightctrl':
            modifier ^= osk.modifierCodes['RCTRL'];
            break;
          case 'ctrl':
            modifier ^= osk.modifierCodes['CTRL'];
            break;
          case 'leftalt':
            modifier ^= osk.modifierCodes['LALT'];
            break;
          case 'rightalt':
            modifier ^= osk.modifierCodes['RALT'];
            break;
          case 'alt':
            modifier ^= osk.modifierCodes['ALT'];
            break;
          default:
            s = id;
        }
        // Combine our base modifiable layer and attach the new modifier variation info to obtain our destination layer.
        if(s != 'default') {
          if(s == '') {
            s = osk.getLayerId(modifier);
          } else {
            s = osk.getLayerId(modifier) + '-' + s;
          }
        }
      }
      
      if(s == '') {
        s = 'default';
      }

      // Actually set the new layer id.
      osk.layerId = s;

      // Check that requested layer is defined   (KMEA-1, but does not resolve issue)
      for(i=0; i<osk.layers.length; i++)
        if(osk.layerId == osk.layers[i].id) return;

      // Show default layer if an undefined layer has been requested
      osk.layerId='default';
    }

    /**
     * Indicate the current language and keyboard on the space bar
     **/
    osk.showLanguage = function() {
      var lgName='',kbdName='';
      var activeStub = keymanweb.keyboardManager.activeStub;

      if(activeStub) {
        lgName=activeStub['KL'];
        kbdName=activeStub['KN'];
      } else if(keymanweb._ActiveLanguage) {
        lgName=keymanweb._ActiveLanguage['KN'];
      } else {
        lgName='English';
      }

      try
      {
        var t=osk.spaceBar.firstChild.firstChild;
        if(typeof(t.parentNode.className) == 'undefined' || t.parentNode.className == '')
          t.parentNode.className='kmw-spacebar';
        else
          t.parentNode.className +=' kmw-spacebar';

        t.className='kmw-spacebar-caption';
        kbdName=kbdName.replace(/\s*keyboard\s*/i,'');

        // We use a separate variable here to keep down on MutationObserver messages in keymanweb.js code.
        var keyboardName = "";
        if(kbdName == lgName) {
          keyboardName=lgName;
        } else {
          keyboardName=lgName+' ('+kbdName+')';
        }
        // It sounds redundant, but this dramatically cuts down on browser DOM processing.
        if(t.innerHTML != keyboardName) {
          t.innerHTML = keyboardName;
        }
      }
      catch(ex){}
    }

    /**
     * Display list of installed keyboards in pop-up menu
     **/
    osk.showLanguageMenu = function()
    {
      var n=0,kbdList=keymanweb.keyboardManager.keyboardStubs,nKbds=kbdList.length;
      if(nKbds < 1) return;

      // Create the menu list container element
      var menu=osk.lgList=util._CreateElement('DIV'),ss;
      osk.lgList.id='kmw-language-menu';

      // Insert a transparent overlay to prevent anything else happening during keyboard selection,
      // but allow the menu to be closed if anywhere else on screen is touched
      menu.shim=util._CreateElement('DIV');
      menu.shim.id='kmw-language-menu-background';
      menu.shim.addEventListener('touchstart',
        function(e)
        {
          e.preventDefault(); osk.hideLanguageList();

          // Display build only if touching menu, space *and* one other point on screen (build 369)
          if(e.touches.length > 2)
          {
            var sX=e.touches[1].pageX,sY=e.touches[1].pageY;
            if(sX > osk.spaceBar.offsetLeft && sX < osk.spaceBar.offsetLeft+osk.spaceBar.offsetWidth &&
              sY > osk.spaceBar.offsetTop && sY < osk.spaceBar.offsetTop+osk.spaceBar.offsetHeight) osk.showBuild();
          }
        },false);
      document.body.appendChild(menu.shim);

      // Add two nested DIVs to properly support iOS scrolling with momentum
      //  c.f. https://github.com/joelambert/ScrollFix/issues/2
      var m2=util._CreateElement('DIV'),s2=m2.style,
          m3=util._CreateElement('DIV'),s3=m3.style;
      m2.id='kmw-menu-scroll-container'; m3.id='kmw-menu-scroller';

      // Support momentum scrolling on iOS
      if('WebkitOverflowScrolling' in s2) s2.WebkitOverflowScrolling='touch';

      m2.appendChild(m3);
      menu.appendChild(m2);

      // Add menu index strip
      var i,x,mx=util._CreateElement('DIV');
      mx.id='kmw-menu-index';
      for(i=1; i<=26; i++)
      {
        x=util._CreateElement('P');
        x.innerHTML=String.fromCharCode(i+64);
        mx.appendChild(x);
      }

      // Add index selection (for a large menu)
      mx.addEventListener('touchstart',function(e){osk.scrollToLanguage(e,m2,m3);},false);
      mx.addEventListener('touchend',function(e){e.stopPropagation();e.preventDefault();},false);
      menu.appendChild(mx);

  //TODO: not sure if either of these two handlers ar actually needed.  touchmove handler may be doing all that is necessary.
      // Add scroll end event handling to override body scroll
      menu.addEventListener('scroll',function(e){
        osk.lgList.scrolling=true;
        },false);
      m2.addEventListener('scroll',function(e){
        //osk.lgList.scrolling=true;
        if(m2.scrollTop < 1)m2.scrollTop=1;
        if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1)m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;
        },false);

      // Add a list of keyboards to the innermost DIV
      osk.lgList.activeLgNo=osk.addLanguagesToMenu(m3,kbdList);

      // Get number of visible (language) selectors
      var nLgs=m3.childNodes.length-1;

      // Do not display until sizes have been calculated
      osk.lgList.visibility='hidden';

      // Append menu to document body, not to OSK
      document.body.appendChild(osk.lgList);

      // Adjust size for viewport scaling (probably not needed for iOS, but check!)
      if(device.OS == 'Android' && 'devicePixelRatio' in window)
        osk.lgList.style.fontSize=(2/window.devicePixelRatio)+'em';


      // Adjust width for pixel scaling on Android tablets
      if(device.OS == 'Android' && device.formFactor == 'tablet' && 'devicePixelRatio' in window)
      {
        var w=parseInt(util.getStyleValue(menu,'width'),10),ms=menu.style;
        if(!isNaN(w)) ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
        w=parseInt(util.getStyleValue(m2,'width'),10); ms=m2.style;
        if(!isNaN(w)) ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
        w=parseInt(util.getStyleValue(m3,'width'),10); ms=m3.style;
        if(!isNaN(w)) ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
      }

      // Adjust initial top and height of menu
      osk.adjustLanguageMenu(0);

      // Adjust the index font size and line height
      var dy=mx.childNodes[1].offsetTop-mx.childNodes[0].offsetTop,
          lineHeight=Math.floor(menu.offsetHeight/26.0),
          scale=Math.round(100.0*lineHeight/dy)/100.0,
          factor=(scale > 0.6 ? 1 : 2);

      if(scale > 1.25) scale=1.25;

      for(i=0;i<26;i++)
      {
        var qs=mx.childNodes[i].style;
        if(factor == 2 && (i%2) == 1)
        {
          qs.display='none';
        }
        else
        {
          qs.fontSize=(scale*factor)+'em';
          qs.lineHeight=(lineHeight*factor)+'px';
        }
      }

      // Increase width of outer menu DIV by index, else hide index
      var menuWidth=m2.offsetWidth;
      if(m2.scrollHeight > m2.offsetHeight+3)
        menuWidth = menuWidth+mx.offsetWidth;
      else
        mx.style.display='none';

      menu.style.width=menuWidth+'px';

      // Now display the menu
      osk.lgList.visibility='';

      // Set initial scroll to show current language (but never less than 1, to avoid dragging body)
      var top=m3.firstChild.offsetHeight*osk.lgList.activeLgNo+1;
      m2.scrollTop=top;

      // The scrollTop value is limited by the device, and must be limited to avoid dragging the document body
      if(m2.scrollTop < top) m2.scrollTop=m2.scrollHeight-m2.offsetHeight;
      if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1)m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;

    }

    /**
     * Adjust top and height of language menu
     *
     * @param   {number}  nKbds number of displayed keyboards to add to number of languages
     **/
    osk.adjustLanguageMenu = function(nKbds)
    {
      var menu=osk.lgList,m2=menu.firstChild,m3=m2.firstChild,
        barWidth=0,s=menu.style,mx=menu.childNodes[1],
        maxHeight=window.innerHeight-osk.lgKey.offsetHeight-16,
        nItems=m3.childNodes.length+nKbds-1,      // Number of (visible) keyboard selectors
        itemHeight=m3.firstChild.firstChild.offsetHeight,
        menuHeight=nItems*itemHeight;

      // Correct maxheight for viewport scaling (iPhone/iPod only) and internal position corrections
      if(device.OS == 'iOS')
      {
        if(device.formFactor == 'phone')
        {
          barWidth=(util.landscapeView() ? 36 : 0);
          maxHeight=(window.innerHeight-barWidth-16)*util.getViewportScale();
        }
        else if(device.formFactor == 'tablet')
        {
          barWidth=(util.landscapeView() ? 16 : 0);
          maxHeight=(maxHeight-barWidth);
        }
      }

      // Explicitly set position and height
      s.left=util._GetAbsoluteX(osk.lgKey)+'px';
      if(menuHeight > maxHeight) menuHeight=maxHeight;
      s.height=menuHeight+'px';

      // Position menu at bottom of screen, but referred to top (works for both iOS and Firefox)
      s.top=(util._GetAbsoluteY(osk._Box)+osk._Box.offsetHeight-menuHeight+window.pageYOffset-6)+'px';
      s.bottom='auto';

      // Explicitly set the scroller and index heights to the container height
      mx.style.height=m2.style.height=s.height;

    }

    /**
     * Add an index to the language menu
     *
     *  @param  {Object}  e         touch start event from index
     *  @param  {Object}  m2        menu scroller DIV
     *  @param  {Object}  menu      DIV with list of languages
     */
    osk.scrollToLanguage = function(e,m2,menu)
    {
      e.stopImmediatePropagation();e.stopPropagation();e.preventDefault();
      if(e.touches[0].target.nodeName != 'P') return;
      var i,t,top=0,initial=e.touches[0].target.innerHTML.charCodeAt(0),nn=menu.childNodes;
      try {
        for(i=0; i<nn.length-1; i++)
        {
          t=nn[i].firstChild.innerHTML.toUpperCase().charCodeAt(0);
          if(t >= initial) break;
        }
      }
      catch(ex){}
      try
      {
        top=menu.firstChild.offsetHeight*i+1;
        m2.scrollTop=top;
      }
      catch(ex){top=0;}
      try
      {
        if(m2.scrollTop < top) m2.scrollTop=m2.scrollHeight-m2.offsetHeight;
        if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1) m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;
      }
      catch(ex){}
    }

    /**
     * Display all languages for installed keyboards in scrollable list
     *
     *    @param    {Object}    menu      DIV to which language selectors will be added
     *    @param    {Object}    kbdList   array of keyboard stub objects
     *    @return   {number}              index of currently active language
     **/
    osk.addLanguagesToMenu = function(menu,kbdList)
    {
      var nStubs=kbdList.length;

      // Create and sort a list of languages
      var k,n,lg,langs=[];
      for(n=0; n<nStubs; n++)
      {
        lg=kbdList[n]['KL'];
        if(langs.indexOf(lg) == -1) langs.push(lg);
      }
      langs.sort();

      // Get current scale factor (reciprocal of viewport scale)
      var scale=Math.round(100/util.getViewportScale())/100;

      var dx,lgBar,kList,i,kb,activeLanguageIndex=-1;
      for(k=0; k<langs.length; k++)
      {
        dx=util._CreateElement('DIV');dx.className='kbd-list-closed';
        lgBar=util._CreateElement('P');
        lgBar.kList=[];

        for(n=0; n<nStubs; n++)
        {
          if(kbdList[n]['KL'] == langs[k]) lgBar.kList.push(kbdList[n]);
        }

        // Adjust bar size for current viewport scaling (iOS only!)
        if(device.OS == 'iOS') lgBar.style.fontSize=scale+'em';

        // Add to menu
        dx.appendChild(lgBar);
        menu.appendChild(dx);

        if(langs[k] == (<KeymanBase>keymanweb).keyboardManager.activeStub['KL']) activeLanguageIndex=k;

        // Several keyboards for this language
        if(lgBar.kList.length > 1)
        {
          lgBar.className='kbd-list';
          lgBar.innerHTML=langs[k]+'...';
          lgBar.scrolled=false;
          lgBar.ontouchend=function(e)
          {
            e.preventDefault();e.stopPropagation();
            if(e.target.scrolled)
              e.target.scrolled=false;
            else
              this.parentNode.className=(this.parentNode.className=='kbd-list-closed'?'kbd-list-open':'kbd-list-closed');

            // Adjust top of menu to allow for expanded list
            osk.adjustLanguageMenu(this.parentNode.className=='kbd-list-closed'?0:this.kList.length);
          }
          lgBar.addEventListener('touchstart',function(e){e.stopPropagation();},false);
          lgBar.addEventListener('touchmove',function(e){e.target.scrolled=true;e.stopPropagation();},false);

          for(i=0; i<lgBar.kList.length; i++)
          {
            kb=util._CreateElement('P');kb.className='kbd-list-entry';
            if(device.OS == 'iOS') kb.style.fontSize=scale+'em';
            osk.addKeyboardToMenu(lgBar.kList[i],kb,false);
            dx.appendChild(kb);
          }
        }

        // Only one keyboard for this language
        else
        {
          lgBar.innerHTML=langs[k];
          lgBar.className='kbd-single-entry';
          osk.addKeyboardToMenu(lgBar.kList[0],lgBar,true);
        }
        if(k == activeLanguageIndex) lgBar.className=lgBar.className+' current';
      }

      // Add a non-selectable bottom bar so to allow scrolling to the last language
      var padLast=util._CreateElement('DIV'); padLast.id='kmw-menu-footer';
      var cancelTouch=function(e){e.preventDefault();e.stopPropagation();};
      padLast.addEventListener('touchstart',cancelTouch,false);
      padLast.addEventListener('touchmove',cancelTouch,false);
      padLast.addEventListener('touchend',cancelTouch,false);
      menu.appendChild(padLast);

      return activeLanguageIndex;
    }

    /**
     * Add a keyboard entry to the language menu *
     *
     * @param   {Object}    kbd     keyboard object
     * @param   {Object}    kb      element being added and styled
     * @param   {boolean}   unique  is this the only keyboard for the language?
     */
    osk.addKeyboardToMenu = function(kbd,kb,unique)
    {
      kb.kn=kbd['KI'];        // InternalName;
      kb.kc=kbd['KLC'];       // LanguageCode;
      kb.innerHTML=unique?kbd['KL']:kbd['KN'].replace(' Keyboard',''); // Name

      // Touchstart (or mspointerdown) event highlights the touched list item
      var touchStart=function(e)
      {
        e.stopPropagation();
        if(this.className.indexOf('selected') <= 0) this.className=this.className+' selected';
        osk.lgList.scrolling=false;
        osk.lgList.y0=e.touches[0].pageY;//osk.lgList.childNodes[0].scrollTop;
        return true;
      },
  //TODO: Still drags Android background sometimes (not consistently)
      // Touchmove drags the list and prevents release from selecting the language
      touchMove=function(e)
      {
        e.stopImmediatePropagation();
        var scroller=osk.lgList.childNodes[0],
            yMax=scroller.scrollHeight-scroller.offsetHeight,
        y, dy;

        if("undefined" != typeof e.pageY) y = e.pageY;
        else if("undefined" != typeof e.touches) y = e.touches[0].pageY;
        else return;

        dy=y-osk.lgList.y0;

        // Scroll up (show later listed languages)
        if(dy < 0)
        {
          if(scroller.scrollTop >= yMax-1)
          {
            e.preventDefault(); osk.lgList.y0=y;
          }
        }
        // Scroll down (show earlier listed languages)
        else if(dy > 0)
        {
          if(scroller.scrollTop < 2)
          {
            e.preventDefault(); osk.lgList.y0=y;
          }
        }
        // Dont' scroll - can happen if changing scroll direction
        else
          return;

        // Disable selected language if drag more than 5px
        if(dy < -5 || dy > 5)
        {
          osk.lgList.scrolling=true;
          this.className=this.className.replace(/\s*selected/,'');
          osk.lgList.y0=y;
        }
        return true;
      },

      // Touch release (click) event selects touched list item
      touchEnd=function(e)
      {
        e.preventDefault();
        if(typeof(e.stopImmediatePropagation) != 'undefined') e.stopImmediatePropagation();else e.stopPropagation();

        if(osk.lgList.scrolling)
        {
          this.className=this.className.replace(/\s*selected/,'');
        }
        else
        {
          com.keyman.DOMEventHandlers.states.setFocusTimer();

          osk.lgList.style.display='none'; //still allows blank menu momentarily on selection
          keymanweb.keyboardManager._SetActiveKeyboard(this.kn,this.kc,true);
          keymanweb.keyboardManager.doKeyboardChange(this.kn,this.kc);
          keymanweb.domManager.focusLastActiveElement();
          osk.hideLanguageList();

          // Update the OSK with the new keyboard
          osk._Show();
        }
        return true;
      };

      kb.onmspointerdown=touchStart;
      kb.addEventListener('touchstart',touchStart,false);
      kb.onmspointermove=touchMove;
      kb.addEventListener('touchmove',touchMove,false);
      kb.onmspointerout=touchEnd;
      kb.addEventListener('touchend',touchEnd,false);
    }

    /**
     * Remove the language menu again
     **/
    osk.hideLanguageList = function()
    {
      if(osk.lgList)
      {
        osk.highlightKey(osk.lgKey.firstChild,false);
        osk.lgList.style.visibility='hidden';
        window.setTimeout(function(){
        if(osk.lgList != null && typeof osk.lgList != 'undefined') {
            document.body.removeChild(osk.lgList.shim);
            document.body.removeChild(osk.lgList);
      }
          osk.lgList=null;
          },500);
      }
    }

    /**
     * Function     _UpdateVKShift
     * Scope        Private
     * @param       {Object}            e     OSK event
     * @param       {number}            v     keyboard shift state
     * @param       {(boolean|number)}  d     set (1) or clear(0) shift state bits
     * @return      {boolean}                 Always true
     * Description  Update the current shift state within KMW
     */
    osk._UpdateVKShift = function(e, v, d) {
      var keyShiftState=0, lockStates=0, i;

      var lockNames  = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'];
      var lockKeys   = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];

      if(e) {
        // read shift states from Pevent
        keyShiftState = e.Lmodifiers;
        lockStates = e.Lstates;

        // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
        if(keymanweb.keyboardManager.isChiral() && osk.emulatesAltGr() && 
            (com.keyman.DOMEventHandlers.states.modStateFlags & osk.modifierBitmasks['ALT_GR_SIM']) == osk.modifierBitmasks['ALT_GR_SIM']) {
          keyShiftState |= osk.modifierBitmasks['ALT_GR_SIM'];
          keyShiftState &= ~osk.modifierCodes['RALT'];
        }

        for(i=0; i < lockNames.length; i++) {
          if(lockStates & osk.stateBitmasks[lockNames[i]]) {
            osk._stateKeys[lockKeys[i]] = lockStates & osk.modifierCodes[lockNames[i]];
          }
        }
      } else if(d) {
        keyShiftState |= v;

        for(i=0; i < lockNames.length; i++) {
          if(v & osk.stateBitmasks[lockNames[i]]) {
            osk._stateKeys[lockKeys[i]] = true;
          }
        }
      } else {
        keyShiftState &= ~v;

        for(i=0; i < lockNames.length; i++) {
          if(v & osk.stateBitmasks[lockNames[i]]) {
            osk._stateKeys[lockKeys[i]] = false;
          }
        }
      }

      // Find and display the selected OSK layer
      osk.layerId=osk.getLayerId(keyShiftState);

      // osk._UpdateVKShiftStyle will be called automatically upon the next _Show.
      if(osk._Visible) {
        osk._Show();
      }

      return true;
    }

    /**
     * Function     _UpdateVKShiftStyle
     * Scope        Private
     * @param       {string=}   layerId
     * Description  Updates the OSK's visual style for any toggled state keys
     */
    osk._UpdateVKShiftStyle = function(layerId) {
      var i, n, layer=null, layerElement=null;

      if(layerId) {
        for(n=0; n<osk.layers.length; n++) {
          if(osk.layers[n]['id'] == osk.layerId) {
            break;
          }
        }

        return;  // Failed to find the requested layer.
      } else {
        n=osk.layerIndex;
        layerId=osk.layers[n]['id'];
      }

      layer=osk.layers[n];
      
      // Set the on/off state of any visible state keys.
      var states =['K_CAPS',      'K_NUMLOCK',  'K_SCROLL'];
      var keys   =[layer.capsKey, layer.numKey, layer.scrollKey];

      for(i=0; i < keys.length; i++) {
        // Skip any keys not in the OSK!
        if(keys[i] == null) {
          continue;
        }

        keys[i]['sp'] = osk._stateKeys[states[i]] ? osk.buttonClasses['SHIFT-ON'] : osk.buttonClasses['SHIFT'];
        var btn = document.getElementById(layerId+'-'+states[i]);

        osk.setButtonClass(keys[i], btn, osk.layout);
      }
    }

    /**
     * Function     _CancelMouse
     * Scope        Private
     * @param       {Object}      e     event
     * @return      {boolean}           always false
     * Description  Closes mouse click event
     */
    osk._CancelMouse=function(e)
    {
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(e && e.preventDefault) e.preventDefault();
      if(e) { e.cancelBubble=true; e.returnValue=false; } // I2409 - Avoid focus loss for visual keyboard events
      return false;
    }

    /**
     * Function     showLayer
     * Scope        Private
     * @param       {string}      id      ID of the layer to show
     * @return      {boolean}             true if the layer is shown, or false if it cannot be found
     * Description  Shows the layer identified by 'id' in the on screen keyboard
     */
    osk.showLayer = function(id)
    {
      if(keymanweb.keyboardManager.activeKeyboard)
      {
        for(var i=0; i<osk.layers.length; i++)
        {
          if(osk.layers[i].id == id)
          {
            osk.layerId=id;
            osk._Show();
            return true;
          }
        }
      }
      return false;
    }

    /**
     * Get the wanted height of the OSK for touch devices
     *
     *  @return   {number}    height in pixels
     **/
    osk.getHeight=function()
    {

      // KeymanTouch - get OSK height from device
      if(typeof(keymanweb['getOskHeight']) == 'function') return keymanweb['getOskHeight']();

      var oskHeightLandscapeView=Math.floor(Math.min(screen.availHeight,screen.availWidth)/2),
          height=oskHeightLandscapeView;

      if(device.formFactor == 'phone')
      {
        var sx=Math.min(screen.height,screen.width),
            sy=Math.max(screen.height,screen.width);

        if(util.portraitView())
          height=Math.floor(Math.max(screen.availHeight,screen.availWidth)/3);
        else
          height=height*(sy/sx)/1.6;  //adjust for aspect ratio, increase slightly for iPhone 5
      }

      // Correct for viewport scaling (iOS - Android 4.2 does not want this, at least on Galaxy Tab 3))
      if(device.OS == 'iOS') height=height/util.getViewportScale();

      // Correct for devicePixelratio - needed on Android 4.1.2 phones,
      // for Opera, Chrome and Firefox, but not for native browser!   Exclude native browser for Build 344.
      if(device.OS == 'Android' && device.formFactor == 'phone' && 'devicePixelRatio' in window)
      {
        var bMatch=/Firefox|Chrome|OPR/;
        if(bMatch.test(navigator.userAgent)) height = height*window.devicePixelRatio;
      }

      return height;
    }

    /**
     * Get the wanted width of the OSK for touch devices
     *
     *  @return   {number}    height in pixels
     **/
    osk.getWidth=function()
    {
      // KeymanTouch - get OSK height from device
      if(typeof(keymanweb['getOskWidth']) == 'function') return keymanweb['getOskWidth']();

      // screen width always correct for iOS (iOS 8, anyway)
      var width=screen.width;

      // On Android (Opera, Chrome and Firefox; native browser not supported)
      // must use document client width, using window width only if document undefined
      if(device.OS == 'Android')
      {
        try {
          width=document.documentElement.clientWidth;
          }
        catch(ex) {
          width=screen.availWidth;
          }
      }
      return width;
    }

    /**
     * Mouse down/mouse over event handler (desktop only)
     *
     * @param   {Event}  e  mouse over/mouse down event object
     */
    osk.mouseOverMouseDownHandler = function(e)
    {
      var t=util.eventTarget(e);
      if(t === null || device.formFactor != 'desktop') return;

      if(t.nodeName == 'SPAN') t=t.parentNode;
      if(util.eventType(e) == 'mousedown')
      {
        osk.currentKey=t.id; osk._CancelMouse(e);
        osk.highlightKey(t,true);
      }
      else if(t.id == osk.currentKey)
      {
        osk.highlightKey(t,true);
      }
    }

    /**
     * Mouse up/mouse out event handler (desktop only)
     *
     * @param   {Event}  e  mouse up/out event object
     */
    osk.mouseUpMouseOutHandler = function(e)
    {
      var t=util.eventTarget(e);
      if(t === null || device.formFactor != 'desktop') return;

      if(t.nodeName == 'SPAN') t=t.parentNode;
      osk.highlightKey(t,false);

      // Process as click if mouse button released anywhere over key
      if(util.eventType(e) == 'mouseup')
      {
          if(t.id == osk.currentKey) osk.clickKey(t);
          osk.currentKey='';
      }
    }

    /**
     * Returns the default properties for a key object, used to construct
     * both a base keyboard key and popup keys
     *
     * @return    {Object}    An object that contains default key properties
     */
    osk.getDefaultKeyObject = function() {
      return {
        'text': '',
        'width': '100',
        'pad': '15',
        'sp': '0',
        'layer': null,
        'nextlayer': null
      };
    };

    /**
     * Create the OSK for a particular keyboard and device
     *
     * @param       {Object}              layout      OSK layout definition
     * @param       {string}              formFactor  layout form factor
     * @return      {Object}                          fully formatted OSK object
     */
    osk.deviceDependentLayout=function(layout,formFactor)
    {
        var lDiv=util._CreateElement('DIV'),ls=lDiv.style,actualHeight=0;

        // Set OSK box default style
        lDiv.className='kmw-key-layer-group';

        // Adjust OSK height for mobile and tablet devices TODO move outside this function???
        switch(formFactor)
        {
          case 'phone':
          case 'tablet':
            actualHeight=osk.getHeight();
            ls.height=actualHeight+'px';
            break;
        }

        // Return empty DIV if no layout defined
        if(layout == null) return lDiv;

        // Set default OSK font size (Build 344, KMEW-90)
        if(typeof(layout['fontsize']) == 'undefined' || layout['fontsize'] == null)
          ls.fontSize='1em';
        else
          ls.fontSize=layout['fontsize'];

        osk.fontSize=ls.fontSize;       //TODO: move outside function*********

        // Create a separate OSK div for each OSK layer, only one of which will ever be visible
        var n,i,j,layers,layer,gDiv,rows,row,rowHeight,rDiv,keys,key,kDiv,pDiv,rs,ks,btn,bs,ps,gs;

        layers=layout['layer'];

        // Set key default attributes (must use exportable names!)
        var tKey=osk.getDefaultKeyObject();
        tKey['fontsize']=ls.fontSize;

        // Identify key labels (e.g. *Shift*) that require the special OSK font
        var specialLabel=/\*\w+\*/;

        // ***Delete any empty rows at the end added by compiler bug...
        for(n=0; n<layers.length; n++)
        {
          layer=layers[n]; rows=layer['row'];
          for(i=rows.length; i>0; i--)
          {
            if(rows[i-1]['key'].length > 0) break;
          }
          if(i < rows.length) rows.splice(i-rows.length,rows.length-i);
        }
        // ...remove to here when compiler bug fixed ***

        // Set the OSK row height, **assuming all layers have the same number of rows**

        // Calculate default row height
        rowHeight=100/rows.length;

        // For desktop OSK, use a percentage of the OSK height
        if(formFactor == 'desktop')
        {
          rowHeight=100/rows.length;
        }

        // Get the actual available document width and scale factor according to device type
        var objectUnits, objectWidth;
        if(formFactor == 'desktop') {
          objectUnits = function(v) { return v + '%' };
          objectWidth = 100;
        } else {
          objectUnits = function(v) { return Math.round(v)+'px' };
          objectWidth = osk.getWidth();
        }

        if(device.touchable) //  /*&& ('ontouchstart' in window)*/ // Except Chrome emulation doesn't set this.
        {                                                          // Not to mention, it's rather redundant.
          lDiv.addEventListener('touchstart', osk.touch,true);
          // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
          lDiv.addEventListener('touchend', osk.release,true); 
          lDiv.addEventListener('touchmove', osk.moveOver,false);
          //lDiv.addEventListener('touchcancel', osk.cancel,false); //event never generated by iOS
        }

        for(n=0; n<layers.length; n++)
        {
          layer=layers[n];
          layer.aligned=false;
          gDiv=util._CreateElement('DIV'),gs=gDiv.style;
          gDiv.className='kmw-key-layer';

          // Always make the first layer visible
          gs.display=(n==0?'block':'none');
          gs.height=ls.height;

          // Set font for layer if defined in layout
          if('font' in layout) gs.fontFamily=layout['font']; else gs.fontFamily='';

          gDiv.layer=gDiv.nextLayer=layer['id'];
          if(typeof layer['nextlayer'] == 'string') gDiv.nextLayer=layer['nextlayer'];

          // Create a DIV for each row of the group
          rows=layer['row'];

          // Calculate the maximum row width (in layout units)
          var totalWidth=0;
          for(i=0; i<rows.length; i++)
          {
            var width=0;
            row=rows[i]; keys=row['key'];
            for(j=0; j<keys.length; j++)
            {
              key=keys[j];

              // Test for a trailing comma included in spec, added as null object by IE
              if(key == null)
              {
                keys.length = keys.length-1;
              }
              else
              {
                var kw, kp;
                kw = (typeof key['width'] == 'string' && key['width'] != '') ? parseInt(key['width'],10) : 100;
                if(isNaN(kw) || kw == 0) kw = 100;
                key['width'] = kw.toString();
                kp = (typeof key['pad'] == 'string' && key['pad'] != '') ? parseInt(key['pad'],10) : 15;
                if(isNaN(kp) || kp == 0) kp = 15;  // KMEW-119
                key['pad'] = kp.toString();
                width += kw + kp;
                //if(typeof key['width'] == 'string' && key['width'] != '') width += parseInt(key['width'],10); else width += 100;
                //if(typeof key['pad'] == 'string' && key['pad'] != '') width += parseInt(key['pad'],10); else width += 5;
              }
            }
            if(width > totalWidth) totalWidth = width;
          }

          // Add default right margin
          if(formFactor == 'desktop') {
            totalWidth += 5;  // KMEW-117
          } else {
            // TODO: Not entirely clear why this needs to be 15 instead of 5 on touch layouts.  We probably have
            // a miscalculation somewhere
            totalWidth += 15;
          }

          for(i=0; i<rows.length; i++)
          {
            rDiv=util._CreateElement('DIV');
            rDiv.className='kmw-key-row';
            // The following event trap is needed to prevent loss of focus in IE9 when clicking on a key gap.
            // Unclear why normal _CreateElement prevention of loss of focus does not seem to work here.
            // Appending handler to event handler chain does not work (other event handling remains active).
            rDiv.onmousedown=util.mouseDownPreventDefaultHandler; // Build 360
            //util.attachDOMEvent(rDiv,'mousedown',function(e){if(e)e.preventDefault();

            row=rows[i]; rs=rDiv.style;

            // Set row height. (Phone and tablet heights are later recalculated
            // and set in px, allowing for viewport scaling.)
            rs.maxHeight=rs.height=rowHeight+'%';

            // Apply defaults, setting the width and other undefined properties for each key
            keys=row['key'];
            for(j=0; j<keys.length; j++)
            {
              key=keys[j];
              for(var tp in tKey)
              {
                if(typeof key[tp] != 'string') key[tp]=tKey[tp];
              }

              // Modify the key type for special keys with non-standard labels
              // to allow the keyboard font to ovveride the SpecialOSK font.
              // Blank keys are no longer reclassed - can use before/after CSS to add text
              switch(key['sp'])
              {
                case '1':
                  if(!specialLabel.test(key['text']) && key['text'] != '') key['sp']='3';
                  break;
                case '2':
                  if(!specialLabel.test(key['text']) && key['text'] != '') key['sp']='4';
                  break;
              }
            }

            // Calculate actual key widths by summing defined widths and scaling each key to %,
            // adjusting the width of the last key to make the total exactly 100%
            // Save each percentage key width as a separate member (do *not* overwrite layout specified width!)
            // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
            // All key widths and paddings are rounded for uniformity
            var keyPercent,padPercent,totalPercent=0;
            for(j=0; j<keys.length-1; j++)
            {
              keyPercent=Math.round(parseInt(keys[j]['width'],10)*objectWidth/totalWidth);
              keys[j]['widthpc']=keyPercent;
              padPercent=Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
              keys[j]['padpc']=padPercent;
              totalPercent += padPercent+keyPercent;
            }

            // Allow for right OSK margin (15 layout units)
            totalPercent += Math.round(15*objectWidth/totalWidth);

            // If a single key, and padding is negative, add padding to right align the key
            if(keys.length == 1 && parseInt(keys[0]['pad'],10) < 0)
            {
              keyPercent=Math.round(parseInt(keys[0]['width'],10)*objectWidth/totalWidth);
              keys[0]['widthpc']=keyPercent;
              totalPercent += keyPercent;
              keys[0]['padpc']=(objectWidth-totalPercent);
            }
            else if(keys.length > 0)
            {
              j=keys.length-1;
              padPercent=Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
              keys[j]['padpc']=padPercent;
              totalPercent += padPercent;
              keys[j]['widthpc']=(objectWidth-totalPercent);
            }

            //Create the key square (an outer DIV) for each key element with padding, and an inner DIV for the button (btn)
            totalPercent=0;
            for(j=0; j<keys.length; j++)
            {
              key=keys[j];
              kDiv=util._CreateElement('DIV');
              kDiv.keyId=key['id'];
              kDiv.className='kmw-key-square';
              ks=kDiv.style;

              kDiv.width=ks.width=objectUnits(key['widthpc']);

              if(formFactor != 'desktop')
              {
                // Regularize interkey spacing by rounding key width and padding (Build 390)
                //keys[j]['padpc']=Math.round(keys[j]['padpc']);
                //keys[j]['widthpc']=Math.round(keys[j]['widthpc']);
                ks.left=objectUnits(totalPercent+keys[j]['padpc']);
                ks.bottom=rs.bottom;
                ks.height=rs.height;  //must be specified in px for rest of layout to work correctly
              }
              else
              {
                ks.marginLeft=objectUnits(key['padpc']);
              }

              totalPercent=totalPercent+keys[j]['padpc']+keys[j]['widthpc'];

              btn=util._CreateElement('DIV');

              // Set button class
              osk.setButtonClass(key,btn,layout);

              // Set distinct phone and tablet button position properties
              if(formFactor != 'desktop')
              {
                btn.style.left=ks.left;
                btn.style.width=ks.width;
              }

              // Add the (US English) keycap label for desktop OSK or if KDU flag is non-zero
              var q=null;
              if(layout.keyLabels || (formFactor == 'desktop')) //desktop or KDU flag set
              {
                // Create the default key cap labels (letter keys, etc.)
                var x=osk.keyCodes[key.id];
                switch(x)
                {
                  case 186: x=59; break;
                  case 187: x=61; break;
                  case 188: x=44; break;
                  case 189: x=45; break;
                  case 190: x=46; break;
                  case 191: x=47; break;
                  case 192: x=96; break;
                  case 219: x=91; break;
                  case 220: x=92; break;
                  case 221: x=93; break;
                  case 222: x=39; break;
                  default:
                    if(x < 48 || x > 90) x=0;
                }

                if(x > 0)
                {
                  q=util._CreateElement('DIV');
                  q.className='kmw-key-label';
                  q.innerHTML=String.fromCharCode(x);
                  //kDiv.appendChild(q);
                  btn.appendChild(q);
                }
              }

              // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
              btn.id=layer['id']+'-'+key.id;
              btn.key=key;  //attach reference to key layout spec to element

              // Add reference to subkey array if defined
              if(typeof key['sk'] != 'undefined' && key['sk'] != null)
              {
                var bsn,bsk=btn.subKeys=key['sk'];
                for(bsn=0; bsn<bsk.length; bsn++)
                  if(bsk[bsn]['sp'] == '1' || bsk[bsn]['sp'] == '2')
                  {
                    var oldText=bsk[bsn]['text'];
                    bsk[bsn]['text']=osk.renameSpecialKey(oldText);
                  }
              }
              else btn.subKeys=null;

              // Define callbacks to handle key touches: iOS and Android tablets and phones
              // TODO: replace inline function calls??
              if(!device.touchable)
              {
                // Highlight key while mouse down or if moving back over originally selected key
                btn.onmouseover=btn.onmousedown=osk.mouseOverMouseDownHandler; // Build 360

                // Remove highlighting when key released or moving off selected element
                btn.onmouseup=btn.onmouseout=osk.mouseUpMouseOutHandler; //Build 360
              }

              // Add OSK key labels
              var t=util._CreateElement('SPAN'),ts=t.style;
              if(key['text'] == null || key['text'] == '')
              {
                t.innerHTML='\xa0';
                if(typeof key['id'] == 'string')
                {
                  if(/^U_[0-9A-F]{4}$/i.test(key['id']))
                    t.innerHTML=String.fromCharCode(parseInt(key['id'].substr(2),16));
                }
              }
              else t.innerHTML=key['text'];
              t.className='kmw-key-text';

              // Use special case lookup for modifier keys
              if(key['sp'] == '1' || key['sp'] == '2')
              {
                var tId=((key['text'] == '*Tab*' && n == 1) ? '*TabLeft*' : key['text']);
                t.innerHTML=osk.renameSpecialKey(tId);
              }

              //Override font spec if set for this key in the layout
              if('font' in key) ts.fontFamily=key['font'];
              if('fontsize' in key) ts.fontSize=key['fontsize'];

              // Add text to button and button to placeholder div
              btn.appendChild(t);
              kDiv.appendChild(btn);

              // Prevent user selection of key captions
              //t.style.webkitUserSelect='none';

              // If a subkey array is defined, add an icon
              if(typeof(key['sk']) != 'undefined' && key['sk'] != null)
              {
                var skIcon=util._CreateElement('DIV');
                skIcon.className='kmw-key-popup-icon';
                //kDiv.appendChild(skIcon);
                btn.appendChild(skIcon);
              }
              // Add key to row
              rDiv.appendChild(kDiv);
            }
            // Add row to layer
            gDiv.appendChild(rDiv);
          }
          // Add layer to group
          lDiv.appendChild(gDiv);
        }
        return lDiv;
    }

    /**
     * Replace default key names by special font codes for modifier keys
     *
     *  @param    {string}  oldText
     *  @return {string}
     **/
    osk.renameSpecialKey = function(oldText) {
      // If a 'special key' mapping exists for the text, replace it with its corresponding special OSK character.
      return osk.specialCharacters[oldText] ? String.fromCharCode(0XE000 + osk.specialCharacters[oldText]) : oldText;
    }

    osk.clearPopup = function()
    {
      // Remove the displayed subkey array, if any, and cancel popup request
      var sk=document.getElementById('kmw-popup-keys');
      if(sk != null)
      {
        if(sk.shim) osk._Box.removeChild(sk.shim);
        sk.parentNode.removeChild(sk);
      }

      if(osk.popupCallout) osk._Box.removeChild(osk.popupCallout);
      osk.popupCallout = null;

      if(osk.subkeyDelayTimer)
      {
          window.clearTimeout(osk.subkeyDelayTimer);
          osk.subkeyDelayTimer = null;
      }
      osk.popupBaseKey = null;
    }

    /**
     * OSK touch start event handler
     *
     *  @param  {Event} e   touch start event object
     *
     */
    osk.touch = function(e)
    {
      // Identify the key touched
      var t = e.changedTouches[0].target, key = osk.keyTarget(t);

      // Save the touch point
      osk.touchX = e.changedTouches[0].pageX;

      // Set the key for the new touch point to be current target, if defined
      osk.currentTarget = key;

      // Prevent multi-touch if popup displayed
      var sk = document.getElementById('kmw-popup-keys');
      if((sk && sk.style.visibility == 'visible') || osk.popupVisible) return;

      // Keep track of number of active (unreleased) touch points
      osk.touchCount = e.touches.length;

      // Get nearest key if touching a hidden key or the end of a key row
      if((key && (key.className.indexOf('key-hidden') >= 0))
        || t.className.indexOf('kmw-key-row') >= 0) {
        key = osk.findNearestKey(e,t);
      }
      // Do not do anything if no key identified!
      if(key == null) {
        return;
      }

      // Get key name (K_...) from element ID
      var keyIdComponents = key.id.split('-');
      var keyName=keyIdComponents[keyIdComponents.length-1];

      // Highlight the touched key
      osk.highlightKey(key,true);

      // Special function keys need immediate action
      if(keyName == 'K_LOPT' || keyName == 'K_ROPT')
      {
        window.setTimeout(function(){osk.clickKey(key);},0);
        osk.keyPending = null;
      }
      // Also backspace, to allow delete to repeat while key held
      else if(keyName == 'K_BKSP') {
        // While we could inline the execution of the delete key here, we lose the ability to
        // record the backspace key if we do so.
        osk.clickKey(key);
        osk.deleteKey = key;
        osk.deleting = window.setTimeout(osk.repeatDelete,500);
        osk.keyPending = null;
      } else {
        if(osk.keyPending) {
          osk.highlightKey(osk.keyPending,false);
          osk.clickKey(osk.keyPending);
          osk.clearPopup();
          // Decrement the number of unreleased touch points to prevent
          // sending the keystroke again when the key is actually released
          osk.touchCount--;
        } else {
          // If this key has subkey, start timer to display subkeys after delay, set up release
          osk.touchHold(key);
          //if(key.subKeys != null) osk.subkeyDelayTimer=window.setTimeout(function(){osk.showSubKeys(key);},osk.popupDelay);
        }
        osk.keyPending = key;
      }
    }

    /**
     * OSK touch release event handler
     *
     *  @param  {Event} e   touch release event object
     *
     **/
    osk.release = function(e)
    {
      // Prevent incorrect multi-touch behaviour if native or device popup visible
      var sk = document.getElementById('kmw-popup-keys'), t = osk.currentTarget;
      if((sk && sk.style.visibility == 'visible') || osk.popupVisible)
      {
        // Ignore release if a multiple touch
        if(e.touches.length > 0) return;

        // Cancel (but do not execute) pending key if neither a popup key or the base key
        if((t == null) || ((t.id.indexOf('popup') < 0) && (t.id != osk.popupBaseKey.id)))
        {
          osk.highlightKey(osk.keyPending,false);
          osk.clearPopup();
          osk.keyPending = null;
        }
      }

      // Handle menu key release event
      if(t && t.id) osk.optionKey(e,t.id,false);

      // Test if moved off screen (effective release point must be corrected for touch point horizontal speed)
      // This is not completely effective and needs some tweaking, especially on Android
      var x = e.changedTouches[0].pageX,
          beyondEdge = ((x < 2 && osk.touchX > 5) ||
            (x > window.innerWidth - 2 && osk.touchX < window.innerWidth - 5));

      // Save then decrement current touch count
      var tc=osk.touchCount;
      if(osk.touchCount > 0) osk.touchCount--;

      // Process and clear highlighting of pending target
      if(osk.keyPending)
      {
        osk.highlightKey(osk.keyPending,false);

        // Output character unless moved off key
        if(osk.keyPending.className.indexOf('hidden') < 0 &&
            tc > 0 && !beyondEdge) osk.clickKey(osk.keyPending);
        osk.clearPopup();
        osk.keyPending = null;
      }
      // Always clear highlighting of current target on release (multi-touch)
      else
      {
        var tt = e.changedTouches[0];
        t = osk.keyTarget(tt.target);
        if(!t)
        {
          var t1 = document.elementFromPoint(tt.clientX,tt.clientY);
          t = osk.findNearestKey(e,t1);
        }
        osk.highlightKey(t,false);
      }

      // Clear repeated backspace if active
      if(osk.deleting) window.clearTimeout(osk.deleting);
      osk.deleting = 0;
    }

    /**
     * OSK touch move event handler
     *
     *  @param  {Event} e   touch move event object
     *
     **/
    osk.moveOver = function(e)
    {
      e.preventDefault(); e.cancelBubble=true;
      if(typeof e.stopImmediatePropagation == 'function') e.stopImmediatePropagation();
      else if(typeof e.stopPropagation == 'function') e.stopPropagation();

      // Do not attempt to support reselection of target key for overlapped keystrokes
      if(e.touches.length > 1 || osk.touchCount == 0) return;

      // Get touch position
      var x=typeof e.touches == 'object' ? e.touches[0].clientX : e.clientX,
          y=typeof e.touches == 'object' ? e.touches[0].clientY : e.clientY;

      // Move target key and highlighting
      var t = e.changedTouches[0],
          t1 = document.elementFromPoint(x,y),
          key0 = osk.keyPending,
          key1 = osk.keyTarget(t1);

      // Find the nearest key to the touch point if not on a visible key
      if((key1 && key1.className.indexOf('key-hidden') >= 0) ||
        (t1 && (!key1) && t1.className.indexOf('key-row') >= 0))
      {
          key1 = osk.findNearestKey(e,t1);
      }

      // Stop repeat if no longer on BKSP key
      if(key1 && (typeof key1.id == 'string') && (key1.id.indexOf('BKSP') < 0))
      {
        if(osk.deleting) window.clearTimeout(osk.deleting);
        osk.deleting = 0;
      }

      // Do not move over keys if device popup visible
      if(osk.popupVisible)
      {
        if(key1 == null)
        {
          if(key0) osk.highlightKey(key0,false);
          osk.keyPending=null;
        }
        else
        {
          if(key1 == osk.popupBaseKey)
          {
            if(!osk.hasClass(key1,'kmw-key-touched')) osk.highlightKey(key1,true);
            osk.keyPending = key1;
          }
          else
          {
            if(key0) osk.highlightKey(key0,false);
            osk.keyPending = null;
          }
        }
        return;
      }

      // Use the popup duplicate of the base key if a phone with a visible popup array
      var sk=document.getElementById('kmw-popup-keys');
      if(sk && sk.style.visibility == 'visible'
        && device.formFactor == 'phone' && key1 == osk.popupBaseKey)
      {
        key1 = sk.childNodes[0].firstChild;
      }

      // Identify current touch position (to manage off-key release)
      osk.currentTarget = key1;

      // Clear previous key highlighting
      if(key0 && key1 && (key1.id != key0.id)) osk.highlightKey(key0,false);

      // If popup is visible, need to move over popup, not over main keyboard
      osk.highlightSubKeys(key1,x,y);

      if(sk && sk.style.visibility == 'visible')
      {
        if(key1 && key1.id.indexOf('popup') < 0 && key1 != osk.popupBaseKey) return;
        if(key1 && key1 == osk.popupBaseKey && key1.className.indexOf('kmw-key-touched') < 0) osk.highlightKey(key1,true);
      }
      // Cancel touch if moved up and off keyboard, unless popup keys visible
      else
      {
        var yMin = Math.max(5,osk._Box.offsetTop - 0.25*osk._Box.offsetHeight);
        if(key0 && e.touches[0].pageY < Math.max(5,osk._Box.offsetTop - 0.25*osk._Box.offsetHeight))
        {
          osk.highlightKey(key0,false); osk.showKeyTip(null,false); osk.keyPending = null;
        }
      }

      // Replace the target key, if any, by the new target key
      // Do not replace a null target, as that indicates the key has already been released
      if(key1 && osk.keyPending) osk.keyPending = key1;

      if(osk.keyPending)
      {
        if(key0 != key1 || key1.className.indexOf('kmw-key-touched') < 0) osk.highlightKey(key1,true);
      }

      if(key0 && key1 && (key1 != key0) && (key1.id != ''))
      {
        //  Display the touch-hold keys (after a pause)
        osk.touchHold(key1);
        /*
      // Clear and restart the popup timer
        if(osk.subkeyDelayTimer)
        {
          window.clearTimeout(osk.subkeyDelayTimer);
          osk.subkeyDelayTimer = null;
        }
        if(key1.subKeys != null)
        {
          osk.subkeyDelayTimer = window.setTimeout(
            function()
            {
              osk.clearPopup();
              osk.showSubKeys(key1);
            },
            osk.popupDelay);
        }
        */
      }
    }

    // osk.cancel = function(e) {} //cancel event is never generated by iOS

    /**
     * More reliable way of identifying  element class
     * @param   {Object}  e HTML element
     * @param   {string}  name  class name
     * @return  {boolean}
     */
    osk.hasClass = function(e, name)
    {
      var className = " " + name + " ";
      return (" " + e.className + " ").replace(/[\n\t\r\f]/g, " ").indexOf(className) >= 0;
    }

    /**
     * Get the current key target from the touch point element within the key
     *
     * @param   {Object}  t   element at touch point
     * @return  {Object}      the key element (or null)
     **/
    osk.keyTarget = function(t)
    {
      try {
        if(t)
        {
          if(osk.hasClass(t,'kmw-key')) return t;
          if(t.parentNode && osk.hasClass(t.parentNode,'kmw-key')) return t.parentNode;
          if(t.firstChild && osk.hasClass(t.firstChild,'kmw-key')) return t.firstChild;
        }
      } catch(ex){}
      return null;
    }

    /**
     * Identify the key nearest to the touch point if at the end of a key row,
     * but return null more than about 0.6 key width from the nearest key.
     *
     *  @param  {Event}   e   touch event
     *  @param  {Object}  t   HTML object at touch point
     *  @return {Object}      nearest key to touch point
     *
     **/
    osk.findNearestKey = function(e,t)
    {
      if((!e) || (typeof e.changedTouches == 'undefined')
        || (e.changedTouches.length == 0)) return null;

      // Get touch point on screen
      var x = e.changedTouches[0].pageX;

      // Get key-row beneath touch point
      while(t && t.className.indexOf('key-row') < 0) t = t.parentNode;
      if(!t) return null;

      // Find minimum distance from any key
      var k, k0=0, dx, dxMax=24, dxMin=100000, x1, x2;
      for(k = 0; k < t.childNodes.length; k++)
      {
        if(t.childNodes[k].firstChild.className.indexOf('key-hidden') >= 0) continue;
        x1 = t.childNodes[k].offsetLeft;
        x2 = x1 + t.childNodes[k].offsetWidth;
        dx =x1 - x;
        if(dx >= 0 && dx < dxMin)
        {
          k0 = k; dxMin = dx;
        }
        dx = x - x2;
        if(dx >= 0 && dx < dxMin)
        {
          k0 = k; dxMin = dx;
        }
      }
      if(dxMin < 100000)
      {
        t = t.childNodes[k0];
        x1 = t.offsetLeft;
        x2 = x1 + t.offsetWidth;

        // Limit extended touch area to the larger of 0.6 of key width and 24 px
        if(t.offsetWidth > 40) dxMax = 0.6 * t.offsetWidth;
        if(((x1 - x) >= 0 && (x1 - x) < dxMax) ||
            ((x - x2) >= 0 && (x - x2) < dxMax))
          return t.firstChild;
      }
      return null;
    }

    /**
     *  Repeat backspace as long as the backspace key is held down
     **/
    osk.repeatDelete = function() {
      if(osk.deleting) {
        osk.clickKey(osk.deleteKey);
        osk.deleting = window.setTimeout(osk.repeatDelete,100);
      }
    }

    /**
     * Attach appropriate class to each key button, according to the layout
     *
     * @param       {Object}    key     key object
     * @param       {Object}    btn     button object
     * @param       {Object=}   layout  source layout description (optional, sometimes)
     */
    osk.setButtonClass = function(key,btn,layout) {
      var n=0, keyTypes=['default','shift','shift-on','special','special-on','','','','deadkey','blank','hidden'];
      if(typeof key['dk'] == 'string' && key['dk'] == '1') { 
        n=8;
      }

      if(typeof key['sp'] == 'string') {
        n=parseInt(key['sp'],10);
      } 

      if(n < 0 || n > 10) {
        n=0;
      }

      layout=layout||osk.layout;

      // Apply an overriding class for 5-row layouts
      var nRows=layout['layer'][0]['row'].length;
      if(nRows > 4 && util.device.formFactor == 'phone') {
        btn.className='kmw-key kmw-5rows kmw-key-'+keyTypes[n];
      } else {
        btn.className='kmw-key kmw-key-'+keyTypes[n];
      }
    }

    /**
     * Converts the legacy BK property from pre 10.0 into the KLS keyboard layer spec format,
     * sparsifying it as possible to pre-emptively check invalid layers.
     * 
     * @param   {Array}   BK      keyboard object (as loaded)
     * @return  {Object}
     */
    osk.processLegacyDefinitions = function(BK) {
      //['default','shift','ctrl','shiftctrl','alt','shiftalt','ctrlalt','shiftctrlalt'];
      var idList=osk.generateLayerIds(false); // Non-chiral.

      var KLS = {};

      // The old default:  eight auto-managed layers...
      for(var n=0; n<idList.length; n++) {
        var id = idList[n], arr = [], valid = false;

        // ... with keycode mappings in blocks of 65.
        for(var k=0; k < 65; k++) {
          var index = k + 65 * n;
          arr.push(BK[index]);

          // The entry for K_SPACE's keycode tends to hold ' ' instead of '', which causes
          // the whole layer to be treated as 'valid' if not included in the conditional.
          if(index < BK.length && BK[index] != '' && k != dfltCodes.indexOf('K_SPACE')) {
            valid = true;
          }
        }

        if(valid) {
          KLS[id] = arr;
        }
      }

      // There must always be at least a plain 'default' layer.  Array(65).fill('') would be preferable but isn't supported on IE, 
      // but buildDefaultLayer will set the defaults for these layers if no entry exists for them in the array due to length.
      if(typeof KLS['default'] == 'undefined' || ! KLS['default']) {
        KLS['default'] = [''];
      }

      // There must always be at least a plain 'shift' layer.
      if(typeof KLS['shift'] == 'undefined' || ! KLS['shift']) {
        KLS['shift'] = [''];
      }

      return KLS;
    }

    /**
     * Sets a formatting property for the modifier keys when constructing a default layout for a keyboard.
     * 
     * @param   {Object}    layer   // One layer specification
     * @param   {boolean}   chiral  // Whether or not the keyboard uses chiral modifier information.
     * @param   {string}    formFactor  // The form factor of the device the layout is being constructed for.
     * @param   {boolean}   key102      // Whether or not the extended key 102 should be hidden.
     */
    osk.formatDefaultLayer = function(layer, chiral, formFactor, key102) {
      var layerId = layer['id'];

      // Correct appearance of state-dependent modifier keys according to group
      for(var i=0; i<layer['row'].length; i++) {
        var row=layer['row'][i];
        var keys=row['key'];
        for(var j=0; j<keys.length; j++) {
          var key=keys[j];
          switch(key['id']) {
            case 'K_SHIFT':
            case 'K_LSHIFT':
            case 'K_RSHIFT':
              if(layerId.indexOf('shift') != -1) {
                key['sp'] = osk.buttonClasses['SHIFT-ON'];
              } 
              if((formFactor != 'desktop') && (layerId != 'default')) {
                key['nextlayer']='default';
              }
              break;
            case 'K_LCTRL':
            case 'K_LCONTROL':
              if(chiral) {
                if(layerId.indexOf('leftctrl') != -1) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];
                }
                break;
              } 
            case 'K_RCTRL':
            case 'K_RCONTROL':
              if(chiral) {
                if(layerId.indexOf('rightctrl') != -1) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];
                }
                break;
              }
            case 'K_CONTROL':
              if(layerId.indexOf('ctrl') != -1) {
                if(!chiral || (layerId.indexOf('leftctrl') != -1 && layerId.indexOf('rightctrl') != -1)) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];              
                }
              }
              break;
            case 'K_LALT':
              if(chiral) {
                if(layerId.indexOf('leftalt') != -1) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];
                }
                break;
              } 
            case 'K_RALT':
              if(chiral) {
                if(layerId.indexOf('rightalt') != -1) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];
                }
                break;
              } 
            case 'K_ALT':
              if(layerId.indexOf('alt') != -1) {
                if(!chiral || (layerId.indexOf('leftalt') != -1 && layerId.indexOf('rightalt') != -1)) {
                  key['sp'] = osk.buttonClasses['SHIFT-ON'];              
                }
              }
              break;
            case 'K_oE2':
              if(typeof key102 == 'undefined' || !key102) {
                if(formFactor == 'desktop') {
                  keys.splice(j--, 1);
                  keys[0]['width']='200';
                } else {
                  keys[j]['sp']=osk.buttonClasses['HIDDEN'];
                }
              }
              break;
          }
        }
      }
    }

    /**
     * Generates a list of potential layer ids for the specified chirality mode.
     * 
     * @param   {boolean|number}   chiral    // Does the keyboard use chiral modifiers or not?
     */
    osk.generateLayerIds = function(chiral) {
      var layerCnt, offset;

      if(chiral) {
        layerCnt=32;
        offset=0x01;
      } else {
        layerCnt=8;
        offset=0x10;
      }

      var layerIds = [];

      for(var i=0; i < layerCnt; i++) {
        layerIds.push(osk.getLayerId(i * offset));
      }

      return layerIds;
    }

    /**
     * Signifies whether or not the OSK facilitates AltGr / Right-alt emulation for this keyboard.
     * @param   {Object=}   keyLabels
     * @return  {boolean}
     */
    osk.emulatesAltGr = function(keyLabels) {
      var layers;

      // If we're not chiral, we're not emulating.
      if(!keymanweb.keyboardManager.isChiral()) {
        return false;
      }

      if(!keyLabels) {
        var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;
        if(activeKeyboard == null || activeKeyboard['KV'] == null) {
          return false;
        }
        
        layers = activeKeyboard['KV']['KLS'];
      } else {
        layers = keyLabels;
      }

      var emulationMask = osk.modifierCodes['LCTRL'] | osk.modifierCodes['LALT'];

      var unshiftedEmulationLayer = layers[osk.getLayerId(emulationMask)];
      var shiftedEmulationLayer = layers[osk.getLayerId(osk.modifierCodes['SHIFT'] | emulationMask)];
      
      // buildDefaultLayout ensures that these are aliased to the original modifier set being emulated.
      // As a result, we can directly test for reference equality.
      if(unshiftedEmulationLayer != null && 
          unshiftedEmulationLayer != layers[osk.getLayerId(osk.modifierCodes['RALT'])]) {
        return false;
      }

      if(shiftedEmulationLayer != null && 
          shiftedEmulationLayer != layers[osk.getLayerId(osk.modifierCodes['RALT'] | osk.modifierCodes['SHIFT'])]) {
        return false;
      }

      // It's technically possible for the OSK to not specify anything while allowing chiral input.  A last-ditch catch:

      var bitmask = keymanweb.keyboardManager.getKeyboardModifierBitmask();
      if((bitmask & emulationMask) != emulationMask) {
        // At least one of the emulation modifiers is never used by the keyboard!  We can confirm everything's safe.
        return true;
      }

      if(unshiftedEmulationLayer == null && shiftedEmulationLayer == null) {
        // We've run out of things to go on; we can't detect if chiral AltGr emulation is intended or not.
        if(!osk.altGrWarning) {
          console.warn("Could not detect if AltGr emulation is safe, but defaulting to active emulation!")
          // Avoid spamming the console with warnings on every call of the method.
          osk.altGrWarning = true;
        }
      }
      return true;
    }

    /**
     * Build a default layout for keyboards with no explicit layout
     *
     * @param   {Object}  PVK         keyboard object (as loaded)
     * @param   {Number}  kbdBitmask  keyboard modifier bitmask
     * @param   {string}  formFactor
     * @return  {Object}
     */
    osk.buildDefaultLayout = function(PVK,kbdBitmask,formFactor)
    {
      var layout;

      // Build a layout using the default for the device
      var layoutType=formFactor, dfltLayout=keymanweb['dfltLayout'];
      if(typeof dfltLayout[layoutType] != 'object') {
        layoutType = 'desktop';
      }

      // Clone the default layout object for this device
      layout=util.deepCopy(dfltLayout[layoutType]);

      var n,layers=layout['layer'], keyLabels=PVK['KLS'], key102=PVK['K102'];
      var i, j, k, m, row, rows, key, keys;
      var chiral = (kbdBitmask & osk.modifierBitmasks.IS_CHIRAL);

      var kmw10Plus = !(typeof keyLabels == 'undefined' || !keyLabels);
      if(!kmw10Plus) {
        // Save the processed key label information to the keyboard's general data.
        // Makes things more efficient elsewhere and for reloading after keyboard swaps.
        keyLabels = PVK['KLS'] = osk.processLegacyDefinitions(PVK['BK']);
      }

      // Identify key labels (e.g. *Shift*) that require the special OSK font
      var specialLabel=/\*\w+\*/;

      // *** Step 1:  instantiate the layer objects. ***

      // Get the list of valid layers, enforcing that the 'default' layer must be the first one processed.
      var validIdList = Object.getOwnPropertyNames(keyLabels), invalidIdList = [];
      validIdList.splice(validIdList.indexOf('default'), 1);
      validIdList = [ 'default' ].concat(validIdList);

      // Automatic AltGr emulation if the 'leftctrl-leftalt' layer is otherwise undefined.
      if(osk.emulatesAltGr(keyLabels)) {
        // We insert only the layers that need to be emulated.
        if((validIdList.indexOf('leftctrl-leftalt') == -1) && validIdList.indexOf('rightalt') != -1) {
          validIdList.push('leftctrl-leftalt');
          keyLabels['leftctrl-leftalt'] = keyLabels['rightalt'];
        }

        if((validIdList.indexOf('leftctrl-leftalt-shift') == -1) && validIdList.indexOf('rightalt-shift') != -1) {
          validIdList.push('leftctrl-leftalt-shift');
          keyLabels['leftctrl-leftalt-shift'] = keyLabels['rightalt-shift'];
        }
      }

      // For desktop devices, we must create all layers, even if invalid.
      if(formFactor == 'desktop') {
        invalidIdList = osk.generateLayerIds(chiral);

        // Filter out all ids considered valid.  (We also don't want duplicates in the following list...)
        for(n=0; n<invalidIdList.length; n++) {
          if(validIdList.indexOf(invalidIdList[n]) != -1) {
            invalidIdList.splice(n--, 1);
          }
        }
      }

      // This ensures all 'valid' layers are at the front of the layer array and managed by the main loop below.
      // 'invalid' layers aren't handled by the loop and thus remain blank after it.
      var idList = validIdList.concat(invalidIdList);

      if(kmw10Plus && formFactor != 'desktop') { // KLS exists, so we know the exact layer set.
        // Find the SHIFT key...
        var shiftKey = null;

        rows = layers[0]['row'];
        for(var r=0; r < rows.length; r++) {
          keys = rows[r]['key'];
          for(var c=0; c < keys.length; c++) {
            key = keys[c];
            if(key['id'] == 'K_SHIFT') {
              shiftKey = key;
            }
          }
        }

        if(shiftKey) {
          // Erase the legacy shifted subkey array.
          shiftKey['sk'] = [];

          for(var layerID in keyLabels) {            
            if(layerID == 'default' || layerID == 'shift') {
              // These two are accessible from the layer without subkeys.
              continue;
            }

            // Create a new subkey for the specified layer so that it will be accessible via OSK.
            var specialChar = osk.modifierSpecials[layerID];
            shiftKey['sk'].push(new com.keyman.OSKKeySpec("K_" + specialChar, specialChar, null, "1", layerID));
          }
        } else {
          // Seriously, this should never happen.  It's here for the debugging log only.
          console.warn("Error in default layout - cannot find default Shift key!");
        }
      }

      for(n=0; n<idList.length; n++) {
        // Populate non-default (shifted) keygroups
        if(n > 0) {
          layers[n]=util.deepCopy(layers[0]);
        }
        layers[n]['id']=idList[n];
        layers[n]['nextlayer']=idList[n]; // This would only be different for a dynamic keyboard

        // Extraced into a helper method to improve readability.
        osk.formatDefaultLayer(layers[n], chiral != 0, formFactor, !!key102);
      }

      // *** Step 2: Layer objects now exist; time to fill them with the appropriate key labels and key styles ***
      for(n=0; n<layers.length; n++)
      {
        var layer=layers[n], kx, shiftKey=null, nextKey=null, allText='';
        var capsKey = null, numKey = null, scrollKey = null;  // null if not in the OSK layout.
        var layerSpec = keyLabels[layer['id']];
        var isShift = layer['id'] == 'shift' ? 1 : 0;
        var isDefault = layer['id'] == 'default' || isShift ? 1 : 0;

        rows=layer['row'];
        for(i=0; i<rows.length; i++)
        {
          keys=rows[i]['key'];
          for(j=0; j<keys.length; j++)
          {
            key=keys[j];
            kx=dfltCodes.indexOf(key['id']);

            // Only create keys for defined layers.  ('default' and 'shift' are always defined.)
            if(layerSpec || isDefault) {
              // Get keycap text from visual keyboard array, if defined in keyboard
              if(layerSpec) {
                if(kx >= 0 && kx < layerSpec.length) key['text']=layerSpec[kx];
              }

              // Fall back to US English keycap text as default for the base two layers if not otherwise defined.
              // (Any 'ghost' keys must be explicitly defined in layout for these layers.)
              if(isDefault) {
                if((key['text'] == '' || typeof key['text'] == 'undefined') &&  key['id'] != 'K_SPACE' && kx+65 * isShift < dfltText.length) {
                  key['text']=dfltText[kx+65*isShift];
                }
              }
            }

            // Leave any unmarked key caps as null strings
            if(typeof(key['text']) == 'undefined') { 
              key['text']='';
            }
            // Detect important tracking keys.
            switch(key['id']) {
              case "K_SHIFT":
                shiftKey=key;
                break;
              case "K_TAB":
                nextKey=key;
                break;
              case "K_CAPS":
                capsKey=key;
                break;
              case "K_NUMLOCK":
                numKey=key;
                break;
              case "K_SCROLL":
                scrollKey=key;
                break;
            }

            // Remove pop-up shift keys referencing invalid layers (Build 349)
            if(key['sk'] != null) {
              for(k=0; k<key['sk'].length; k++) {
                if(validIdList.indexOf(key['sk'][k]['nextlayer']) == -1) {
                  key['sk'].splice(k--, 1);
                }
              }

              if(key['sk'].length == 0) {
                key['sk']=null;
              }
            }
          }
        }

        // We're done with the layer keys initialization pass.  Time to do post-analysis layer-level init where necessary.
        layer.shiftKey=shiftKey;
        layer.capsKey=capsKey;
        layer.numKey=numKey;
        layer.scrollKey=scrollKey;

        // Set modifier key appearance and behaviour for non-desktop devices using the default layout
        if(formFactor != 'desktop') {
          if(n > 0 && shiftKey != null) {
            shiftKey['sp']=osk.buttonClasses['SHIFT-ON'];
            shiftKey['sk']=null;
            shiftKey['text'] = osk.modifierSpecials[layers[n].id] ? osk.modifierSpecials[layers[n].id] : "*Shift*";
          }
        }
      }

      return layout;
    }

    /**
     * Function     _GenerateVisualKeyboard
     * Scope        Private
     * @param       {Object}      PVK         Visual keyboard name
     * @param       {Object}      Lhelp       true if OSK defined for this keyboard
     * @param       {Object}      layout0 
     * @param       {Number}      kbdBitmask  Keyboard modifier bitmask
     * Description  Generates the visual keyboard element and attaches it to KMW
     */
    osk._GenerateVisualKeyboard = function(PVK,Lhelp,layout0,kbdBitmask)
    {
      var Ldiv,LdivC,layout=layout0;
      var Lkbd=util._CreateElement('DIV'), oskWidth;//s=Lkbd.style,
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;

      // Build a layout using the default for the device
      if(typeof layout != 'object' || layout == null)
        layout=osk.buildDefaultLayout(PVK,kbdBitmask,device.formFactor);

      // Create the collection of HTML elements from the device-dependent layout object
      osk.layout=layout;
      osk.layers=layout['layer'];

      // Override font if specified by keyboard
      if('font' in layout) osk.fontFamily=layout['font']; else osk.fontFamily='';

      // Set flag to add default (US English) key label if specified by keyboard
      layout.keyLabels=activeKeyboard && ((typeof(activeKeyboard['KDU']) != 'undefined') && activeKeyboard['KDU']);
      LdivC=osk.deviceDependentLayout(layout,device.formFactor);

      osk.ddOSK = true;

      // Append the OSK layer group container element to the containing element
      osk.keyMap = LdivC; Lkbd.appendChild(LdivC);

      // Set base class and box class - OS and keyboard added for Build 360
      osk._DivVKbdHelp = osk._DivVKbd = Lkbd;
      osk._Box.className=device.formFactor+' '+device.OS.toLowerCase()+' kmw-osk-frame';
      Lkbd.className=device.formFactor+' kmw-osk-inner-frame';

      // Add header element to OSK only for desktop browsers
      if(device.formFactor == 'desktop')
        osk._Box.appendChild(osk.controlBar());

      // Add primary keyboard element to OSK
      osk._Box.appendChild(Lkbd);

      // Add footer element to OSK only for desktop browsers
      if(device.formFactor == 'desktop')
        osk._Box.appendChild(osk.resizeBar());

      // For other devices, adjust the object heights, allowing for viewport scaling
      else
        osk.adjustHeights();
    }

    /**
     * Create copy of the OSK that can be used for embedding in documentation or help
     * The currently active keyboard will be returned if PInternalName is null
     *
     *  @param  {string}          PInternalName   internal name of keyboard, with or without Keyboard_ prefix
     *  @param  {number}          Pstatic         static keyboard flag  (unselectable elements)
     *  @param  {string=}         argFormFactor   layout form factor, defaulting to 'desktop'
     *  @param  {(string|number)=}  argLayerId    name or index of layer to show, defaulting to 'default'
     *  @return {Object}                          DIV object with filled keyboard layer content
     */
    keymanweb['BuildVisualKeyboard'] = keymanweb.buildOSK = function(PInternalName,Pstatic,argFormFactor,argLayerId)  // I777
    {
      var PKbd=keymanweb.keyboardManager.activeKeyboard,Ln,kbd=null,
          formFactor=(typeof(argFormFactor) == 'undefined' ? 'desktop' : argFormFactor),
          layerId=(typeof(argLayerId) == 'undefined' ? 'default' : argLayerId);

      var keyboardsList = keymanweb.keyboardManager.keyboards;

      if(PInternalName != null) {
        var p=PInternalName.toLowerCase().replace('keyboard_','');

        for(Ln=0; Ln<keyboardsList.length; Ln++) {
          if(p == keyboardsList[Ln]['KI'].toLowerCase().replace('keyboard_','')) {
            PKbd=keyboardsList[Ln]; break;
          }
        }
      }

      if(!PKbd) return null;

      var layouts=PKbd['KVKL'],layout=null,PVK=PKbd['KV'];

      // Get the layout defined in the keyboard, or its nearest equivalent
      if(typeof layouts == 'object')
      {
        if(typeof(layouts[formFactor]) == 'object' && layouts[formFactor] != null)
          layout=layouts[formFactor];
        else if(formFactor == 'phone' && typeof(layouts['tablet']) == 'object' && layouts['tablet'] != null)
          layout=layouts['tablet'];
        else if(formFactor == 'tablet' && typeof(layouts['phone']) == 'object' && layouts['phone'] != null)
          layout=layouts['phone'];
        else if(typeof(layouts['desktop']) == 'object' && layouts['desktop'] != null)
          layout=layouts['desktop'];
      }

      // Else get a default layout for the device for this keyboard
      if(layout == null && PVK != null)
        layout=osk.buildDefaultLayout(PVK,keymanweb.keyboardManager.getKeyboardModifierBitmask(PKbd),formFactor);

      // Cannot create an OSK if no layout defined, just return empty DIV
      if(layout != null)
        layout.keyLabels=((typeof(PKbd['KDU']) != 'undefined') && PKbd['KDU']);

      kbd=osk.deviceDependentLayout(layout,formFactor);
      kbd.className=formFactor+'-static kmw-osk-inner-frame';

      // Select the layer to display, and adjust sizes
      if(layout != null)
      {
        var layer,row,key,Lr,Lk;
        for(Ln=0; Ln<layout.layer.length; Ln++)
        {
          layer=kbd.childNodes[Ln];
          for(Lr=0; Lr<layer.childNodes.length; Lr++)
          {
            row=layer.childNodes[Lr];
            for(Lk=0; Lk<row.childNodes.length; Lk++)
            {
              key=row.childNodes[Lk];
              key.style.height='100%';
            }
          }
          if(typeof(layerId) == 'number')
            layer.style.display=(Ln == layerId && layerId >= 0 ? 'block' : 'none');
          else if(typeof(layerId) == 'string')
            layer.style.display=(layout.layer[Ln].id == layerId ? 'block' : 'none');
          else
            layer.style.display=(Ln == 0 ? 'block' : 'none');
        }
      }
      else
      {
        kbd.innerHTML="<p style='color:#c40; font-size:0.5em;margin:10px;'>No "+formFactor+" layout is defined for "+PKbd['KN']+".</p>";
      }
      // Add a faint border
      kbd.style.border='1px solid #ccc';
      return kbd;
    }

    /**
     * Adjust the absolute height of each keyboard element after a rotation
     *
     **/
    osk.adjustHeights=function()
    {
    if(!osk._Box || !osk._Box.firstChild || !osk._Box.firstChild.firstChild || !osk._Box.firstChild.firstChild.childNodes) return;
      var layers=osk._Box.firstChild.firstChild.childNodes,
          nRows=layers[0].childNodes.length,
          oskHeight=osk.getHeight(),
          rowHeight=Math.floor(oskHeight/(nRows == 0 ? 1 : nRows)),
          nLayer,nRow,rs,keys,nKeys,nKey,key,ks,j,pad,fs=1.0;

      if(device.OS == 'Android' && 'devicePixelRatio' in window)
        rowHeight = rowHeight/window.devicePixelRatio;

      oskHeight=nRows*rowHeight;

      var b=osk._Box,bs=b.style;
      bs.height=bs.maxHeight=(oskHeight+3)+'px';
      b=b.firstChild.firstChild; bs=b.style;
      bs.height=bs.maxHeight=(oskHeight+3)+'px';
      if(device.formFactor == 'phone') fs=0.6;

      // TODO: Logically, this should be needed for Android, too - may need to be changed for the next version!
      if(device.OS == 'iOS')
        fs=fs/util.getViewportScale();

      bs.fontSize=fs+'em';
      var resizeLabels=(device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());

      for(nLayer=0;nLayer<layers.length; nLayer++)
      {
        // Check the heights of each row, in case different layers have different row counts.
        nRows=layers[nLayer].childNodes.length;
        rowHeight=Math.floor(oskHeight/(nRows == 0 ? 1 : nRows));

        pad = Math.round(0.15*rowHeight);
        layers[nLayer].style.height=(oskHeight+3)+'px';
        for(nRow=0; nRow<nRows; nRow++)
        {
          rs=layers[nLayer].childNodes[nRow].style;
          rs.bottom=(nRows-nRow-1)*rowHeight+1+'px';
          rs.maxHeight=rs.height=rowHeight+'px';
          keys=layers[nLayer].childNodes[nRow].childNodes;
          nKeys=keys.length;
          for(nKey=0;nKey<nKeys;nKey++)
          {
            key=keys[nKey];
            //key.style.marginTop = (device.formFactor == 'phone' ? pad : 4)+'px';
            //**no longer needed if base key label and popup icon are within btn, not container**

            // Must set the height of the btn DIV, not the label (if any)
            for(j=0; j<key.childNodes.length; j++)
              if(osk.hasClass(key.childNodes[j],'kmw-key')) break;
            ks=key.childNodes[j].style;
            ks.bottom=rs.bottom;
            ks.height=ks.minHeight=(rowHeight-pad)+'px';

            // Rescale keycap labels on iPhone (iOS 7)
            if(resizeLabels && (j > 0)) key.childNodes[0].style.fontSize='6px';
          }
        }
      }
    }

    /**
     * Create a control bar with title and buttons for the desktop OSK
     */
    osk.controlBar = function()
    {
      var bar=util._CreateElement('DIV'),title='';
      bar.id='keymanweb_title_bar';
      bar.className='kmw-title-bar';
      bar.onmousedown=osk._VMoveMouseDown;

      if(keymanweb.keyboardManager.activeKeyboard) {
        title=keymanweb.keyboardManager.activeKeyboard['KN'];
      }
      var Ltitle=util._CreateElement('SPAN');
      Ltitle.className='kmw-title-bar-caption';
      Ltitle.innerHTML=title;
      bar.appendChild(Ltitle);

      var Limg=osk.closeButton=util._CreateElement('DIV');
      Limg.id='kmw-close-button';
      Limg.className='kmw-title-bar-image';
      Limg.onmousedown=osk._CancelMouse;
      Limg.onclick=function(){osk._Hide(true);}
      bar.appendChild(Limg);

      Limg=osk.helpImg=util._CreateElement('DIV');
      Limg.id='kmw-help-image';
      Limg.className='kmw-title-bar-image';
      Limg.title='KeymanWeb Help';
      Limg.onclick=function()
        {
          var p={}; util.callEvent('osk.helpclick',p);
          if(window.event) window.event.returnValue=false;
          return false;
        }
      Limg.onmousedown=osk._CancelMouse;
      bar.appendChild(Limg);

      Limg=osk.configImg=util._CreateElement('DIV');
      Limg.id='kmw-config-image';
      Limg.className='kmw-title-bar-image';
      Limg.title='KeymanWeb Configuration Options';
      Limg.onclick=function()
        {
          var p={}; util.callEvent('osk.configclick',p);
          if(window.event) window.event.returnValue=false;
          return false;
        }
      Limg.onmousedown=osk._CancelMouse;
      bar.appendChild(Limg);

      Limg=osk.pinImg=util._CreateElement('DIV');  //I2186
      Limg.id='kmw-pin-image';
      Limg.className='kmw-title-bar-image';
      Limg.title='Pin the On Screen Keyboard to its default location on the active text box';
      Limg.onclick=function()
        {
          osk.loadCookie(); osk.userPositioned=false; osk.saveCookie();
          osk._Show();
          osk.doResizeMove(); //allow the UI to respond to OSK movements
          if(osk.pinImg) osk.pinImg.style.display='none';
          if(window.event) window.event.returnValue=false;
          return false;
        }
      Limg.onmousedown=osk._CancelMouse;
      bar.appendChild(Limg);

      return bar;
    }

    /**
     * Display build number
     */
    osk.showBuild = function()
    {
      util.alert('KeymanWeb Version '+keymanweb['version']+'.'+keymanweb['build']+'<br /><br />'
        +'<span style="font-size:0.8em">Copyright &copy; 2017 SIL International</span>');
    }

    /**
     * Create a bottom bar with a resizing icon for the desktop OSK
     */
    osk.resizeBar = function()
    {
      var bar=util._CreateElement('DIV');
      bar.className='kmw-footer';
      bar.onmousedown=osk._CancelMouse;

      // Add caption
      var Ltitle=util._CreateElement('DIV');
      Ltitle.className='kmw-footer-caption';
      Ltitle.innerHTML='<a href="https://keyman.com/developer/keymanweb/">KeymanWeb</a>';
      Ltitle.id='keymanweb-osk-footer-caption';

      // Display build number on shift+double click
      util.attachDOMEvent(Ltitle,'dblclick',function(e){if(e && e.shiftKey)osk.showBuild();},false);

      // Prevent selection of caption (IE - set by class for other browsers)
      if('onselectstart' in Ltitle) Ltitle.onselectstart= util.selectStartHandler; //IE (Build 360)

      bar.appendChild(Ltitle);

      var Limg = util._CreateElement('DIV');
      Limg.className='kmw-footer-resize';
      Limg.onmousedown=osk._VResizeMouseDown;
      Limg.onmouseover=Limg.onmouseout=osk._VResizeMouseOut;
      bar.appendChild(Limg);
      osk.resizeIcon=Limg;
      //TODO: the image never appears in IE8, have no idea why!
      return bar;
    }

    /**
     * Function     _VKbdMouseOver
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Activate the KMW UI on mouse over
     */
    osk._VKbdMouseOver = function(e)
    {
      keymanweb.uiManager.setActivatingUI(true);
    }

    /**
     * Function     _VKbdMouseOut
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Cancel activation of KMW UI on mouse out
     */
    osk._VKbdMouseOut = function(e)
    {
      keymanweb.uiManager.setActivatingUI(false);
    }

    /**
     * Function     _VResizeMouseOver, _VResizeMouseOut
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process end of resizing of KMW UI
     */
    osk._VResizeMouseOver = osk._VResizeMouseOut = function(e)
    {
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return false;
      if(e  &&  e.preventDefault) e.preventDefault();
      var r=osk.getRect();
      osk.width=r.width; osk.height=r.height;
      e.cancelBubble = true;
      return false;
    }

    /**
     * Function     _VResizeMouseDown
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process resizing of KMW UI
     */
    osk._VResizeMouseDown = function(e)
    {
      keymanweb.uiManager.justActivated = true;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return true;
      osk.resizing = 1;
      var Lposx,Lposy;
      if (e.pageX) {
        Lposx = e.pageX; Lposy = e.pageY;
        }
      else if(e.clientX) {
        Lposx = e.clientX + document.body.scrollLeft;
        Lposy = e.clientY + document.body.scrollTop;
        }
      osk._ResizeMouseX = Lposx;
      osk._ResizeMouseY = Lposy;
      if(document.onmousemove != osk._VResizeMouseMove  &&  document.onmousemove != osk._VMoveMouseMove)  // I1472 - Dragging off edge of browser window causes muckup
      {
        osk._VPreviousMouseMove = document.onmousemove;
        osk._VPreviousMouseUp = document.onmouseup;
      }
      osk._VPreviousCursor = document.body.style.cursor;
      osk._VPreviousMouseButton = (typeof(e.which)=='undefined' ? e.button : e.which);

      osk._VOriginalWidth = osk._DivVKbd.offsetWidth;
      osk._VOriginalHeight = osk._DivVKbd.offsetHeight;
      document.onmousemove = osk._VResizeMouseMove;
      document.onmouseup = osk._VResizeMoveMouseUp;

      if(document.body.style.cursor) document.body.style.cursor = 'se-resize';
      if(e  &&  e.preventDefault) e.preventDefault();
      e.cancelBubble = true;
      return false;
    }

    /**
     * Function     _VResizeMouseMove
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process mouse movement during resizing of OSK
     */
    osk._VResizeMouseMove = function(e)
    {
      var Lposx,Lposy;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return true;
      osk.resizing = 1;

      if(osk._VPreviousMouseButton != (typeof(e.which)=='undefined' ? e.button : e.which)) // I1472 - Dragging off edge of browser window causes muckup
      {
        return osk._VResizeMoveMouseUp(e);
      }
      else
      {
        if (e.pageX) {
          Lposx = e.pageX; Lposy=e.pageY;
          }
        else if (e.clientX) {
          Lposx = e.clientX + document.body.scrollLeft;
          Lposy = e.clientY + document.body.scrollTop;
          }
        var newWidth=(osk._VOriginalWidth + Lposx - osk._ResizeMouseX),
            newHeight=(osk._VOriginalHeight + Lposy - osk._ResizeMouseY);

        // Set the smallest and largest OSK size
        if(newWidth < 0.2*screen.width) newWidth = 0.2*screen.width;
        if(newHeight < 0.1*screen.height) newHeight = 0.1*screen.height;
        if(newWidth > 0.9*screen.width) newWidth=0.9*screen.width;
        if(newHeight > 0.5*screen.height) newWidth=0.5*screen.height;

        // Set OSK width
        osk._DivVKbd.style.width=newWidth+'px';

        // Explicitly change OSK height and font size - cannot safely rely on scaling from font
        osk._DivVKbd.style.height=newHeight+'px';
        osk._DivVKbd.style.fontSize=(newHeight/8)+'px';

        if(e  &&  e.preventDefault) e.preventDefault();
        e.cancelBubble = true;
        return false;
      }
    }

    /**
     * Function     _VMoveMouseDown
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process mouse down on OSK
     */
    osk._VMoveMouseDown = function(e)
    {
      var Lposx, Lposy;
      keymanweb.uiManager.justActivated = true;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return true;

      osk.resizing = 1;
      if (e.pageX)
        { Lposx = e.pageX; Lposy = e.pageY; }
      else if (e.clientX)
        { Lposx = e.clientX + document.body.scrollLeft; Lposy = e.clientY + document.body.scrollTop; }

      if(document.onmousemove != osk._VResizeMouseMove  &&  document.onmousemove != osk._VMoveMouseMove)  // I1472 - Dragging off edge of browser window causes muckup
      {
        osk._VPreviousMouseMove = document.onmousemove;
        osk._VPreviousMouseUp = document.onmouseup;
      }
      osk._VPreviousCursor = document.body.style.cursor;
      osk._VPreviousMouseButton = (typeof(e.which)=='undefined' ? e.button : e.which);

      osk._VMoveX = Lposx - osk._Box.offsetLeft;
      osk._VMoveY = Lposy - osk._Box.offsetTop;

      if(keymanweb.keyboardManager.isCJK()) osk.pinImg.style.left='15px';

      document.onmousemove = osk._VMoveMouseMove;
      document.onmouseup = osk._VResizeMoveMouseUp;
      if(document.body.style.cursor) document.body.style.cursor = 'move';
      if(e  &&  e.preventDefault) e.preventDefault();
      e.cancelBubble = true;
      return false;
    }

    /**
     * Process mouse drag on OSK
     *
     * @param       {Object}      e      event
     */
    osk._VMoveMouseMove = function(e)
    {
      var Lposx, Lposy;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return true;

      if(osk.noDrag) return true;

      osk.resizing = 1;

      osk.userPositioned = true;
      osk.pinImg.style.display='block';

      if(osk._VPreviousMouseButton != (typeof(e.which)=='undefined' ? e.button : e.which)) // I1472 - Dragging off edge of browser window causes muckup
      {
        return osk._VResizeMoveMouseUp(e);
      }
      else
      {
        if (e.pageX)
          { Lposx = e.pageX; Lposy = e.pageY; }
        else if (e.clientX)
          { Lposx = e.clientX + document.body.scrollLeft; Lposy = e.clientY + document.body.scrollTop; }
        osk._Box.style.left = (Lposx-osk._VMoveX)+'px';
        osk._Box.style.top = (Lposy-osk._VMoveY)+'px';

        if(e  &&  e.preventDefault) e.preventDefault();
        var r=osk.getRect();
        osk.width=r.width;osk.height=r.height;
        e.cancelBubble = true;
        return false;
      }
    }

    /**
     * Function     _VResizeMoveMouseUp
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process mouse up during resizing of KMW UI
     */
    osk._VResizeMoveMouseUp = function(e)
    {
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(!e) return true;
      osk.resizing = 0; osk.currentKey=null;
      document.onmousemove = osk._VPreviousMouseMove;
      document.onmouseup = osk._VPreviousMouseUp;
      if(document.body.style.cursor) document.body.style.cursor = osk._VPreviousCursor;
      keymanweb.domManager.focusLastActiveElement();
      if(e  &&  e.preventDefault) e.preventDefault();
      keymanweb.uiManager.justActivated = false;
      keymanweb.uiManager.setActivatingUI(false);
      if(osk._DivVKbd) {
        osk._VOriginalWidth = osk._DivVKbd.offsetWidth;
        osk._VOriginalHeight = osk._DivVKbd.offsetHeight;
        }
      osk.doResizeMove();
      e.cancelBubble = true;
      osk.saveCookie();
      return false;
    }

    /**
     * Function     userPositioned
     * Scope        Public
     * @return      {(boolean|number)}          true if user located
     * Description  Test if OSK window has been repositioned by user
     */
    osk['userLocated'] = function()
    {
      return osk.userPositioned;
    }

    /**
     * Description  Display KMW OSK (at position set in callback to UI)
     * Function     show
     * Scope        Public
     * @param       {(boolean|number)=}      bShow     True to display, False to hide, omitted to toggle
     */
    osk['show'] = function(bShow)
    {
      if(arguments.length > 0)
      {
        osk._Enabled=bShow;
        if(bShow) osk._Show(); else osk._Hide(true);
      }
      else
      {
        if(osk._Visible) osk._Hide(true); else osk._Show();
      }
    }

    /**
     * Allow UI to respond to OSK being shown (passing position and properties)
     *
     * @param       {Object=}       p     object with coordinates and userdefined flag
     * @return      {boolean}
     *
     */
    osk.doShow = function(p)
    {
      return util.callEvent('osk.show',p);
    }

    /**
     * Allow UI to update respond to OSK being hidden
     *
     * @param       {Object=}       p     object with coordinates and userdefined flag
     * @return      {boolean}
     *
     */
    osk.doHide = function(p)
    {
      return util.callEvent('osk.hide',p);
    }

    /**
     * Allow UI to update OSK position and properties
     *
     * @param       {Object=}     p       object with coordinates and userdefined flag
     *
     */
    osk.doResizeMove = function(p)
    {
      return util.callEvent('osk.resizemove',p);
    }


    /**
     * Display KMW OSK at specified position (returns nothing)
     *
     * @param       {number=}     Px      x-coordinate for OSK rectangle
     * @param       {number=}     Py      y-coordinate for OSK rectangle
     */
    osk._Show = function(Px, Py)
    {
      // Do not try to display OSK if undefined, or no active element
      if(osk._Box == null || keymanweb.domManager.getActiveElement() == null) return;

      // Never display the OSK for desktop browsers unless KMW element is focused, and a keyboard selected
      if((!device.touchable) && (keymanweb.keyboardManager.activeKeyboard == null || !osk._Enabled)) return;

      var Ls = osk._Box.style;

      // Do not display OSK until it has been positioned correctly
      if(device.touchable && Ls.bottom == '') Ls.visibility='hidden';

      // The following code will always be executed except for externally created OSK such as EuroLatin
      if(osk.ddOSK)
      {
        // Enable the currently active keyboard layer and update the default nextLayer member
        var n,nLayer=-1,b=osk._DivVKbd.childNodes[0].childNodes;
        for(n=0; n<b.length; n++)
        {
          if(b[n].layer == osk.layerId)
          {
            b[n].style.display='block';
            //b[n].style.visibility='visible';
            osk.nextLayer=osk.layerId;
            osk.layerIndex=nLayer=n;
            if(typeof osk.layers[n]['nextlayer'] == 'string') osk.nextLayer=osk.layers[n]['nextlayer'];

            // If osk._Show has been called, there's probably been a change in modifier or state key state.  Keep it updated!
            osk._UpdateVKShiftStyle();
          }
          else
          {
            b[n].style.display='none';
            //b[n].style.visibility='hidden';
          }
        }

        if(device.touchable)
        {
          Ls.position='fixed';
          Ls.left=Ls.bottom='0px';
          Ls.height=Ls.maxHeight=osk._Box.firstChild.firstChild.style.height;
          Ls.border='none'; Ls.borderTop='1px solid gray';
          osk._Enabled=1; osk._Visible=1; // I3363 (Build 301)

          // Adjust keyboard font sizes
          if(device.formFactor == 'phone') // I3363 (Build 301)
            osk._DivVKbd.style.fontSize='120%'; //'1.875em';
          else
          {
            // The following is a *temporary* fix for small format tablets, e.g. PendoPad
            if(device.OS == 'Android' && device.formFactor == 'tablet'  &&
                parseInt(Ls.height,10) < 300)
              osk._DivVKbd.style.fontSize='120%';
            else
              osk._DivVKbd.style.fontSize='200%'; //'2.5em';
          }
          // Identify and save references to the language key, hide keyboard key, and space bar
          osk.lgKey=osk.getSpecialKey(nLayer,'K_LOPT');     //TODO: should be saved with layer
          osk.hkKey=osk.getSpecialKey(nLayer,'K_ROPT');

          // Always adjust screen height if iPhone or iPod, to take account of viewport changes
          if(device.OS == 'iOS' && device.formFactor == 'phone') osk.adjustHeights();
        }

        // Define for both desktop and touchable OSK
        osk.spaceBar=osk.getSpecialKey(nLayer,'K_SPACE'); //TODO: should be saved with layer
      }

      //TODO: may need to return here for touch devices??
      Ls.display='block'; //Ls.visibility='visible';
      osk.showLanguage();

      if(device.formFactor == 'desktop')
      {
        Ls.position='absolute'; Ls.display='block'; //Ls.visibility='visible';
        Ls.left='0px';
        osk.loadCookie();
        if(Px >= 0) //probably never happens, legacy support only
        {
          Ls.left = Px + 'px'; Ls.top = Py + 'px';
        }
        else
        {
          if(osk.userPositioned)
          {
            Ls.left=osk.x+'px'; Ls.top=osk.y+'px';
          }
          else
          {
            var el=keymanweb.domManager.getActiveElement();
            if(osk.dfltX != '')
              Ls.left=osk.dfltX;
            else if(typeof el != 'undefined' && el != null)
              Ls.left=util._GetAbsoluteX(el)+'px';

            if(osk.dfltY != '')
              Ls.top=osk.dfltY;
            else if(typeof el != 'undefined' && el != null)
              Ls.top=(util._GetAbsoluteY(el)+el.offsetHeight)+'px';
          }
        }
        osk._Enabled=1; osk._Visible=1;
        if(osk._DivVKbd)
        {
          osk.width=osk._DivVKbd.offsetWidth; osk.height=osk._DivVKbd.offsetHeight;
        }

        osk.saveCookie();

        var pin=osk.pinImg;
        if(typeof pin != 'undefined' && pin != null)
          pin.style.display=osk.userPositioned?'block':'none';
      }

      // If OSK still hidden, make visible only after all calculation finished
      if(Ls.visibility == 'hidden')
        window.setTimeout(function(){osk._Box.style.visibility='visible';},0);

      // Allow desktop UI to execute code when showing the OSK
      if(!device.touchable)
      {
        var Lpos={};
        Lpos['x']=osk._Box.offsetLeft;
        Lpos['y']=osk._Box.offsetTop;
        Lpos['userLocated']=osk.userPositioned;
        osk.doShow(Lpos);
      }
    }

    /**
     *  Adjust the width of the last cell in each row for length differences
     *  due to rounding percentage widths to nearest pixel.
     *
     *  @param  {number}  nLayer    Index of currently visible layer
     */
    osk.adjustRowLengths = function(nLayer)
    {
      if(nLayer >= 0) return;   //TODO: TEST ONLY - remove code if not needed

      var maxWidth,layers=osk._DivVKbd.childNodes[0].childNodes;

      if(nLayer < 0 || nLayer >= layers.length || layers[nLayer].aligned) return;

      // Do not try and align if not visible!
      if(layers[nLayer].style.display != 'block') return;

      // Set max width to be 6 px less than OSK layer width (allow for inter-key spacing)
      // TODO: Adjustment needs to be device and orientation specific
      maxWidth=osk._DivVKbd.childNodes[0].offsetWidth-6;

      if(device.OS == 'Windows')
      {
        maxWidth -= util.landscapeView() ? 4: 40;
      }
      var i,rows=layers[nLayer].childNodes,keys,nKeys,lastKey,xMax;
      for(i=0; i<rows.length; i++)
      {
        keys=rows[i].childNodes;
        nKeys=keys.length;
        xMax=keys[nKeys-2].offsetLeft+keys[nKeys-2].offsetWidth;
        lastKey=keys[nKeys-1];
        lastKey.style.width=(maxWidth-xMax)+'px';
      }
      layers[nLayer].aligned=true;
    }

    /**
     *  Clear the row alignment flag for each layer
     *  @return   {number}    number of currently active layer
     *
     */
    osk.resetRowLengths = function()
    {
      var j,layers=osk._DivVKbd.childNodes[0].childNodes,nLayer=-1;
      for(j=0; j<layers.length; j++)
      {
        if(layers[j].style.display == 'block') nLayer=j;
        layers[j].aligned=false;
      }
      return nLayer;
    }

    /**
     *  Set the reference to a special function key for the
     *  currently visible OSK layer
     *
     *  @param    {number}  nLayer  Index of visible layer
     *  @param    {string}  keyId   key identifier
     *  @return   {Object}          Reference to key
     */
    osk.getSpecialKey = function(nLayer,keyId)
    {
      var k,layers,rows,keys;
      layers=osk._DivVKbd.childNodes[0].childNodes;
      if(nLayer >= 0 && nLayer < layers.length)
      {
        // Special function keys will always be in bottom row (must modify code if not)
        rows=layers[nLayer].childNodes;
        keys=rows[rows.length-1].childNodes;
        for(k=0; k<keys.length; k++)
        {
          if(keys[k].keyId == keyId) return keys[k];
        }
      }
      return null;
    }

    /**
     * Function     hide
     * Scope        Public
     * Description  Prevent display of OSK window on focus
     */
    osk['hide'] = function()
    {
      osk._Enabled=0; osk._Hide(true);
    }

    /**
     * Hide Keymanweb On Screen Keyboard
     *
     * @param       {boolean}   hiddenByUser    Distinguish between hiding on loss of focus and explicit hiding by user
     */
    osk._Hide = function(hiddenByUser)
    {
      // The test for CJK languages is necessary to prevent a picklist (displayed in the OSK) from being hidden by the user
      // Once picklist functionality is separated out, this will no longer be needed.
      // Logic is: execute always if hidden on lost focus, but if requested by user, only if not CJK

      // Save current size if visible
      if(osk._Box && osk._Box.style.display == 'block' && osk._DivVKbd)
      {
        osk.width=osk._DivVKbd.offsetWidth; osk.height=osk._DivVKbd.offsetHeight;
      }
      if(hiddenByUser)
      {
        //osk.loadCookie(); // preserve current offset and userlocated state
        osk._Enabled = ((keymanweb.keyboardManager.isCJK() || device.touchable)?1:0); // I3363 (Build 301)
        osk.saveCookie();  // Save current OSK state, size and position (desktop only)
      }
      else if(device.formFactor == 'desktop')
      {
        //Allow desktop OSK to remain visible on blur if body class set
        if(document.body.className.indexOf('osk-always-visible') >= 0) return;
      }

      osk._Visible = 0;
      if(osk._Box && device.touchable && osk._Box.offsetHeight > 0) // I3363 (Build 301)
      {
        var os=osk._Box.style,h=osk._Box.offsetHeight;
        //Firefox doesn't transition opacity if start delay is explicitly set to 0!
        if(typeof(os.MozBoxSizing) == 'string')
          os.transition='opacity 0.8s linear';
        else
          os.transition=os.msTransition=os.WebkitTransition='opacity 0.5s linear 0';

        // Cannot hide the OSK smoothly using a transitioned drop, since for
        // position:fixed elements transitioning is incompatible with translate3d(),
        // and also does not work with top, bottom or height styles.
        // Opacity can be transitioned and is probably the simplest alternative.
        // We must condition on osk._Visible in case focus has since been moved to another
        // input (in which case osk._Visible will be non-zero)
        window.setTimeout(function()
        {
          var os=osk._Box.style;
          if(osk._Visible)
          {
            // Leave opacity alone and clear transition if another element activated
            os.transition=os.msTransition=os.MozTransition=os.WebkitTransition='';
          }
          else
          {
            // Set opacity to zero, should decrease smoothly
            os.opacity='0';

            // Actually hide the OSK at the end of the transition
            osk._Box.addEventListener('transitionend',osk.hideNow,false);
            osk._Box.addEventListener('webkitTransitionEnd',osk.hideNow,false);
          }
        },200);      // Wait a bit before starting, to allow for moving to another element

      }
      else
      {
        if(osk._Box) osk._Box.style.display = 'none';
      }

      // Allow UI to execute code when hiding the OSK
      var p={}; p['HiddenByUser']=hiddenByUser;
      osk.doHide(p);

      // If hidden by the UI, be sure to restore the focus
      if(hiddenByUser) keymanweb.domManager.focusLastActiveElement();
    }

    /**
     * Function     hideNow
     * Scope        Private
     * Description  Hide the OSK unconditionally and immediately, cancel any pending transition
     */
    osk.hideNow = function() // I3363 (Build 301)
    {
      osk._Box.removeEventListener('transitionend',osk.hideNow,false);
      osk._Box.removeEventListener('webkitTransitionEnd',osk.hideNow,false);

      var os=osk._Box.style;
      os.display='none';
      os.opacity='1';
      osk._Visible=0;
      os.transition=os.msTransition=os.mozTransition=os.WebkitTransition='';

      // Remove highlighting from hide keyboard key, if applied
      if(osk.hkKey && typeof(osk.hkKey) != 'undefined') osk.highlightKey(osk.hkKey.firstChild,false);

    }

    // First time initialization of OSK
    osk.prepare = function()
    {
      // Defer loading the OSK until KMW code initialization complete
      if(!keymanweb['initialized'])
      {
        window.setTimeout(osk.prepare,200);
        return;
      }
      // OSK initialization - create DIV and set default styles
      if(!osk.ready)
      {
        osk._Box = util._CreateElement('DIV');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
        document.body.appendChild(osk._Box);

        // Install the default OSK stylesheet
        util.linkStyleSheet(keymanweb.getStyleSheetPath('kmwosk.css'));

        // For mouse click to prevent loss of focus
        util.attachDOMEvent(osk._Box,'mousedown', function(){keymanweb.uiManager.setActivatingUI(true);});

        // And to prevent touch event default behaviour on mobile devices
        // TODO: are these needed, or do they interfere with other OSK event handling ????
        if(device.touchable) // I3363 (Build 301)
        {
          var cancelEventFunc = function(e) {
            if(e.cancelable) {
              e.preventDefault();
            }
            e.stopPropagation();
            return false;
          };
          
          util.attachDOMEvent(osk._Box, 'touchstart', function(e) {
            keymanweb.uiManager.setActivatingUI(true); 
            return cancelEventFunc(e);
          });
          
          util.attachDOMEvent(osk._Box, 'touchend', cancelEventFunc);
          util.attachDOMEvent(osk._Box, 'touchmove', cancelEventFunc);
          util.attachDOMEvent(osk._Box, 'touchcancel', cancelEventFunc);

          // Can only get (initial) viewport scale factor after page is fully loaded!
          osk.vpScale=util.getViewportScale();
        }
      }
      osk.loadCookie();
      osk.ready=true;
    }
    /**
     * Function     _Load
     * Scope        Private
     * Description  OSK initialization when keyboard selected
     */
    osk._Load = function()   // Load Help
    {
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;

      // If _Load called before OSK is ready, must wait and call again
      if(osk._Box == null)
      {
        if(osk.loadRetry >= 99) return; // fail silently, but should not happen
        window.setTimeout(osk._Load,100);
        osk.loadRetry++;
        return;
      }

      osk.loadRetry = 0;

      if(keymanweb._TitleElement) keymanweb._TitleElement.innerHTML = 'KeymanWeb'; // I1972


      osk._Visible=0;  // I3363 (Build 301)
      osk.layerId='default';
      var s=osk._Box.style;
      s.zIndex='9999'; s.display='none'; s.width='auto';
      s.position = (device.formFactor == 'desktop' ? 'absolute' : 'fixed');

      // Use smaller base font size for mobile devices
      //if(screen.availHeight < 500) s.fontSize='10pt';
      //else if(screen.availHeight < 800) s.fontSize='11pt';
      //else s.fontSize='12pt';
      if(device.formFactor == 'phone') s.fontSize='1.6em';

      osk._DivVKbd = osk._DivVKbdHelp = null;  // I1476 - Handle SELECT overlapping
      osk._Box.innerHTML = '';
      osk._Box.onmouseover = osk._VKbdMouseOver;
      osk._Box.onmouseout = osk._VKbdMouseOut;

      // TODO: find out and document why this should not be done for touch devices!!
      // (Probably to avoid having a null keyboard. But maybe that *is* an option, if there remains a way to get the language menu,
      //  such as a minimized menu button?)
      if(activeKeyboard == null && !device.touchable)
      {
        var Ldiv=util._CreateElement('DIV');
        Ldiv.className = "kmw-title-bar";
        Ldiv.appendChild(osk._TitleBarInterior());
        Ldiv.onmousedown = osk._VMoveMouseDown;
        osk._Box.appendChild(Ldiv);

        Ldiv = util._CreateElement('DIV');
        Ldiv.className='kmw-osk-none';
        osk._Box.appendChild(Ldiv);
      }
      else
      {
        var Lviskbd=null,layouts=null,layout=null,Lhelp='';
        osk._Box.className = "";
        if(activeKeyboard != null)
        {
          Lviskbd=activeKeyboard['KV']; Lhelp=activeKeyboard['KH'];

          // Check if dynamic layout is defined within keyboard
          layouts=activeKeyboard['KVKL'];

          // If any keyboard layout file is provided, use that to override the generated layout
          if(typeof layouts != 'undefined' && layouts != null)
          {
            layout=layouts[device.formFactor];

            // Use the layout for the device, if defined, otherwise use the desktop (default) layout
            if(typeof layout == 'undefined' || layout == null)
            {
              if(device.formFactor == 'phone') layout=layouts['tablet'];
              else if(device.formFactor == 'tablet') layout=layouts['phone'];
              if(typeof layout == 'undefined' || layout == null) layout=layouts['desktop'];
            }
          }
        }

        // Test if Visual keyboard is simply a place holder, set to null if so
        if(Lviskbd != null && Lviskbd['BK'] != null)
        {
          var keyCaps=Lviskbd['BK'],noKeyCaps=true;
          {
            for(var i=0; i<keyCaps.length; i++)
            {
              if(keyCaps[i].length > 0)
              {
                noKeyCaps = false; break;
              }
            }
          }
          if(noKeyCaps) Lviskbd=null;
        }

        // Generate a visual keyboard from the layout (or layout default)
        // TODO: this should probably be unconditional now
        if(Lviskbd != null || Lhelp == '' || device.touchable) // I3363 (Build 301)
        {
          // TODO: May want to define a default BK array here as well
          if(Lviskbd == null) Lviskbd={'F':'Tahoma','BK':dfltText}; //DDOSK

          osk._GenerateVisualKeyboard(Lviskbd, Lhelp, layout, keymanweb.keyboardManager.getKeyboardModifierBitmask());
        }

        else //The following code applies only to preformatted 'help' such as European Latin
        {
          osk.ddOSK = false;
          Ldiv=util._CreateElement('DIV');
          Ldiv.className = "kmw-title-bar";
          Ldiv.appendChild(osk._TitleBarInterior());
          Ldiv.onmousedown = osk._VMoveMouseDown;
          osk._Box.appendChild(Ldiv);

          //Add content
          var Ldiv = util._CreateElement('DIV');
          Ldiv.className='kmw-osk-static';
          Ldiv.innerHTML = Lhelp;
          osk._Box.appendChild(Ldiv);
          if(activeKeyboard['KHF']) activeKeyboard['KHF'](osk._Box);
        }
        if(keymanweb._TitleElement)
        {
          keymanweb._TitleElement.innerHTML = "<span style='font-weight:bold'>"
            + activeKeyboard['KN'] + '</span> - ' + keymanweb._TitleElement.innerHTML; // I1972  // I2186
          keymanweb._TitleElement.className=''; keymanweb._TitleElement.style.color='#fff';
        }
      }

      // Create the key preview (for phones)
      osk.createKeyTip();

      // Correct the classname for the (inner) OSK frame (Build 360)
      var innerFrame=osk._Box.firstChild,
        kbdClass=' kmw-keyboard-'+(activeKeyboard ? activeKeyboard['KI'].replace('Keyboard_','') : '');
      if(innerFrame.id == 'keymanweb_title_bar') innerFrame=innerFrame.nextSibling;
      innerFrame.className='kmw-osk-inner-frame'+kbdClass;

      // Append a stylesheet for this keyboard for keyboard specific styles
      // or if needed to specify an embedded font
      osk.appendStyleSheet();
      if(osk._Enabled) osk._Show();
    }

    /**
     *  Append a style sheet for the current keyboard if needed for specifying an embedded font
     *  or to re-apply the default element font
     *
     **/
    osk.appendStyleSheet = function() {
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;
      var activeStub: com.keyman.KeyboardStub = keymanweb.keyboardManager.activeStub;

      // Do not do anything if a null stub
      if(activeStub == null) {
        return;
      }

      // First remove any existing keyboard style sheet
      if(osk.styleSheet) {
        util.removeStyleSheet(osk.styleSheet);
      }

      var i, kfd=activeStub['KFont'], ofd=activeStub['KOskFont'];

      // Add style sheets for embedded fonts if necessary (each font-face style will only be added once)
      util.addFontFaceStyleSheet(kfd); util.addFontFaceStyleSheet(ofd);

      // Temporarily hide duplicated elements on non-desktop browsers
      keymanweb.alignInputs(false);

      // Build the style string and append (or replace) the font style sheet
      // Note: Some browsers do not download the font-face font until it is applied,
      //       so must apply style before testing for font availability
      // Extended to allow keyboard-specific custom styles for Build 360
      var customStyle=osk.addFontStyle(kfd,ofd);
      if( activeKeyboard != null && typeof(activeKeyboard['KCSS']) == 'string')  // KMEW-129
        customStyle=customStyle+activeKeyboard['KCSS'];

      osk.styleSheet = util.addStyleSheet(customStyle); //Build 360

      // Wait until font is loaded then align duplicated input elements with page elements
      if(osk.waitForFonts(kfd,ofd)) keymanweb.alignInputs(true);
    }

    /**
     *  Add or replace the style sheet used to set the font for input elements and OSK
     *
     *  @param  {Object}  kfd   KFont font descriptor
     *  @param  {Object}  ofd   OSK font descriptor (if any)
     *  @return {string}
     *
     **/
    osk.addFontStyle = function(kfd,ofd)
    {
      // Get name of font to be applied
      var fn=keymanweb.baseFont;
      if(typeof(kfd) != 'undefined' && typeof(kfd['family']) != 'undefined') fn=kfd['family'];

      // Unquote font name in base font (if quoted)
      fn = fn.replace(/\u0022/g,'');

      // Set font family chain for mapped elements and remove any double quotes
      var rx=new RegExp('\\s?'+fn+',?'), ff=keymanweb.appliedFont.replace(/\u0022/g,'');

      // Remove base font name from chain if present
      ff = ff.replace(rx,''); ff = ff.replace(/,$/,'');

      // Then replace it at the head of the chain
      if(ff == '') ff=fn; else ff=fn+','+ff;

      // Re-insert quotes around individual font names
      ff = '"' + ff.replace(/\,\s?/g,'","') + '"';

      // Add to the stylesheet, quoted, and with !important to override any explicit style
      var s='.keymanweb-font{\nfont-family:' + ff + ' !important;\n}\n';

      // Set font family for OSK text
      if(typeof(ofd) != 'undefined')
        s=s+'.kmw-key-text{\nfont-family:"'+ofd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';
      else if(typeof(kfd) != 'undefined')
        s=s+'.kmw-key-text{\nfont-family:"'+kfd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';

      // Store the current font chain (with quote-delimited font names)
      keymanweb.appliedFont = ff;

      // Return the style string
      return s;
    }

    /**
     * Function     _Unload
     * Scope        Private
     * Description  Clears OSK variables prior to exit (JMD 1.9.1 - relocation of local variables 3/9/10)
     */
    osk._Unload = function()
    {
      osk._VShift = osk._DivVKbd = osk._VKeySpans = osk._Box = 0;
    }

    /**
     * Save size, position, font size and visibility of OSK
     */
    osk.saveCookie = function()
    {
      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      var p=osk.getPos();

      c['visible'] = osk._Enabled ? 1 : 0;
      c['userSet'] = osk.userPositioned ? 1 : 0;
      c['left'] = p.left; c['top'] = p.top;
      if(osk._DivVKbd)
      {
        c['width'] = osk.width; c['height'] = osk.height;
      }
      util.saveCookie('KeymanWeb_OnScreenKeyboard',c);
    }

    /**
     * Restore size, position, font size and visibility of desktop OSK
     *
     *  @return {boolean}
     */
    osk.loadCookie = function()
    {
      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      if(typeof(c) == 'undefined' || c == null)
      {
        osk.userPositioned=false; return false;
      }
      osk._Enabled = util.toNumber(c['visible'],true);
      osk.userPositioned = util.toNumber(c['userSet'],false);
      osk.x = util.toNumber(c['left'],-1);
      osk.y = util.toNumber(c['top'],-1);

      // Restore OSK size - font size now fixed in relation to OSK height, unless overridden (in em) by keyboard
      var dfltWidth=0.3*screen.width;
      //if(util.toNumber(c['width'],0) == 0) dfltWidth=0.5*screen.width;
      var newWidth=util.toNumber(c['width'],dfltWidth),
          newHeight=util.toNumber(c['height'],0.15*screen.height);

      // Limit the OSK dimensions to reasonable values
      if(newWidth < 0.2*screen.width) newWidth = 0.2*screen.width;
      if(newHeight < 0.1*screen.height) newHeight = 0.1*screen.height;
      if(newWidth > 0.9*screen.width) newWidth=0.9*screen.width;
      if(newHeight > 0.5*screen.height) newWidth=0.5*screen.height;

      if(osk._DivVKbd)
      {
        osk._DivVKbd.style.width=newWidth+'px';
        osk._DivVKbd.style.height=newHeight+'px';
        osk._DivVKbd.style.fontSize=(newHeight/8)+'px';
      }

      // and OSK position if user located
      if(osk.x == -1 || osk.y == -1 || (!osk._Box)) osk.userPositioned = false;

      if(osk.x < window.pageXOffset-0.8*newWidth) osk.x=window.pageXOffset-0.8*newWidth;
      if(osk.y < 0) {osk.x=-1; osk.y=-1; osk.userPositioned=false;}

      if(osk.userPositioned && osk._Box) osk.setPos({'left':osk.x,'top':osk.y});

      return true;
    }

  })();

}