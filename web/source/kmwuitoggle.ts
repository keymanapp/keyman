/// <reference path="../node_modules/@keymanapp/web-utils/src/kmwstring.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!window['keyman']['ui']['name']) { 
  /********************************/
  /*                              */
  /* Toggle User Interface Code   */
  /*                              */
  /********************************/
      
  /**
   * Do not enclose in an anonymous function, as the compiler may create
   * global scope variables to replace true, false, null, which may collide
   * with other variables.
   * Instead, use the --output-wrapper command during optimization, which will
   * add the anonymous function to enclose all code, including those optimized
   * variables which would otherwise have global scope.
   **/  

  try {

    // Declare KeymanWeb, OnScreen Keyboard and Util objects
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'];
    var dbg=keymanweb['debug'];
    
    // Disable UI for touch devices
    if(util['isTouchDevice']()) throw '';

    // Initialize user interface common variables       
    var ui:any=keymanweb['ui'] = {
      name: 'toggle',
      initialized: false,
      controller: null,
      oskButton: null,
      kbdButton: null,
      controllerHovered: false,
      keyboards: [],
      lastActiveKeyboard: -1,
      selectedMenuItem: null,
      updateList: true,
      updateTimer: null    
    }
    
    /**
     * Update the KeymanWeb user interface when an input element is focused or blurred
     * 
     * @param       {Object}            someElement     focused element
     * @param       {(boolean|number)}  focusing        true if focusing
     * @param       {Object}            activeControl   Object representing API specs for the control, if it exists and is now focused.
     */    
    ui.doFocus = function(someElement,focusing, activeControl) {
      // This callback must be ignored until UI is initialized, or for touch devices (which can never initialize the UI)
      if(!ui.initialized) {
        return;
      }
      
      // We don't want to shift the controller to something that's not an input element,
      // but do want to account for window.event's data when legitimate.
      if(window.event && keymanweb['isAttached'](window.event.srcElement)) {
        someElement=window.event.srcElement;
      }

      if(focusing)
      {
        ui.controller.style.display = 'block';
      } 
      else 
      {
        if(!(keymanweb['getUIState']()['activationPending']) && !ui.controllerHovered)
        {
          ui.controller.style.display = 'none';   
        }
      }
      
    /* I2406 - Find an appropriate position for the controller */  
      var x, y, w, h, p;
    
      p = util['getAbsolute'](someElement); x = p['x']; y=p['y'];

      var ownerDoc = someElement.ownerDocument;
      if(ownerDoc.designMode == 'on' && ownerDoc.defaultView && ownerDoc.defaultView.frameElement)
      { 
        w = ownerDoc.defaultView.frameElement.clientWidth;
        h = ownerDoc.defaultView.frameElement.clientHeight;
      }
      else
      {
        w = someElement.offsetWidth;
        h = someElement.offsetHeight;
      }
      if(x + w > window.innerWidth + document.documentElement.scrollLeft - ui.controller.offsetWidth - 1)
        y += h;
      else
      {
        x += w + 2;  
        //y += h - ui.controller.offsetHeight - 1; // ui.controller.offsetheight is returned as its prior value, which results in incorrect UI positioning
        // so better to set the offset absolutely JMD 11/2/11
        y += h - 29; //prevent UI being positioned *above* the top of the element 
      }
        
      if(isNaN(x) || isNaN(y)) return;
      ui.controller.style.left = x + 'px';
      ui.controller.style.top = y + 'px';    
    }

    keymanweb['addEventListener']('controlfocused',function(params){ui.doFocus(params.target,true, params['activeControl']);});
    keymanweb['addEventListener']('controlblurred',function(params){ui.doFocus(params.target,false, null);});

    osk['addEventListener']('show',
      function(oskPosition)
      {
        // Ensure that the ui.controller is visible if help is displayed
        ui.controller.style.display = 'block';
        ui.oskButton._setSelected(true);
      
  /* The following is probably not needed for KMW2, since OSK position is set by KMW, not by the UI        
        //TODO:  This may not be correct - may need to refer to userLocated argument, not function???
        // Check if OSK position is user-defined, return if so
        if (osk['userLocated']()) return oskPosition;
      
        // Otherwise, return the position with respect to the focussed element
        var someElement = keymanweb['getLastActiveElement'](), x, y, w, h, p;
        if(someElement != null)
        {
          p = util['getAbsolute'](someElement); x = p['x']; y = p['y'];
          if(someElement.parentWindow && someElement.parentWindow.frameElement)
          {
            w = someElement.parentWindow.frameElement.clientWidth;
            h = someElement.parentWindow.frameElement.clientHeight;
          }
          else if(someElement.defaultView && someElement.defaultView.frameElement)
          { 
            w = someElement.defaultView.frameElement.clientWidth;
            h = someElement.defaultView.frameElement.clientHeight;
          }
          else
          {
            w = someElement.offsetWidth;
            h = someElement.offsetHeight;
          }
    //TODO:  check the logic of this - it doesn't look right!!!  Signs on top, height??
          var r = osk['getRect']();
          x += 32;
          if(y + h + (r.height - r.top) > (window.clientHeight?window.clientHeight:window.innerHeight) + document.documentElement.scrollTop && 
            y - (r.height - r.top) >= document.documentElement.scrollTop)
          {
            y -= (r.height - r.top);
          }
          else
            y += h;
        }
        else
        {
          x = ui.controller.offsetLeft;
          y = ui.controller.offsetTop + ui.controller.offsetHeight;
        }
        oskPosition['x'] = x; 
        oskPosition['y'] = y; 
  */      
        return oskPosition;
      });
    
    osk['addEventListener']('hide',
      function(byUser)
      {
        if(byUser['HiddenByUser']) ui.oskButton._setSelected(false);
      });
    
    /**
     * Toggle the on screen keyboard display - KMW button control event 
     **/
    ui.switchOsk = function()
    {
      // Check that user control of OSK is allowed
      if((keymanweb['getActiveKeyboard']() == '') || keymanweb['isCJK']() ) return;  
      
      osk['show'](!osk['isEnabled']());  
    }
    
    /**
     * Toggle a single keyboard on or off - KMW button control event 
     **/
    ui.switchSingleKbd = function()
    {
      var _v = (keymanweb['getActiveKeyboard']() == ''),nLastKbd=0,kbdName='',lgCode='';
      if(_v)
      {
        if(ui.keyboards.length == 0) return;
        if(ui.lastActiveKeyboard < ui.keyboards.length && ui.lastActiveKeyboard >= 0) nLastKbd = ui.lastActiveKeyboard;
        kbdName = ui.keyboards[nLastKbd]._InternalName;
        lgCode = ui.keyboards[nLastKbd]._LanguageCode;
        keymanweb['setActiveKeyboard'](kbdName,lgCode);
        ui.lastActiveKeyboard = nLastKbd;
      }
      else
      {
        keymanweb['setActiveKeyboard']('');
      }
      if(ui.kbdButton) ui.kbdButton._setSelected(_v);
    }
    
    /**
     * Switch to the next keyboard in the list - KMW button control event 
     **/
    ui.switchNextKbd = function()
    {
      var _v = (keymanweb['getActiveKeyboard']() == ''),kbdName='',lgCode='';
      if(_v)
      {
        if(ui.keyboards.length == 0) return;
        kbdName = ui.keyboards[0]._InternalName;
        lgCode = ui.keyboards[0]._LanguageCode;
        keymanweb['setActiveKeyboard'](kbdName,lgCode);
        ui.lastActiveKeyboard = 0;
      }
      else
      {
        if(ui.lastActiveKeyboard == ui.keyboards.length-1)
        {
          keymanweb['setActiveKeyboard']('');
          _v = false;
        }
        else
        {
          kbdName = ui.keyboards[++ui.lastActiveKeyboard]._InternalName;
          lgCode = ui.keyboards[ui.lastActiveKeyboard]._LanguageCode; 
          keymanweb['setActiveKeyboard'](kbdName,lgCode);
          _v = true;
        }
      }
      if(ui.kbdButton) ui.kbdButton._setSelected(_v);
    }
    
    /**
     * Create a button object for KeymanWeb UI buttons
     * 
     * @constructor   
     * @param       {string}    _src
     * @param       {string}    _caption
     * @param       {boolean}   _selected
     * @return      {Object}    
     * 
     * @suppress {suspiciousCode}  // Closure isn't smart enough to realize that _onmouseover
     *                             // and the like are defined on individual instances later.
     *                             // It thinks they're always null.        
     **/
    ui.button = function(_src, _caption, _selected)
    {
      this._onclick = null;
      this._onmouseover = null;
      this._onmouseout = null;
      this._elem = null;
      this._down = false;
      this._over = false;
      this._selected = _selected;
    
      /*public*/ this.getElem = function()
      {
        return this._owningObject._elem;
      };
      
      /*private*/ this.__updatestyle = function()
      {
        var ss=this._owningObject._elem.style;
        if(this._owningObject._over)
        {
          ss.margin = '0px';
          if(this._owningObject._selected) {
            ss.border = 'solid 1px #ad4a28';
            ss.background = '#dfb4b4';
          }
          else {
            ss.border = 'solid 1px #dfb4b4';
            ss.background = '#f3e5de';
          }
        }
        else if(this._owningObject._selected)
        {
          ss.background = '#f3e5de';
          ss.margin = '0px';
          ss.border = 'solid 1px #ad4a28';
        }
        else
        {
          ss.background = 'none';
          ss.margin = '1px';
          ss.border = 'none';
        }
      };
      
      /*private*/ this.__mouseover = function()
      {
        ui.controllerHovered = true;
        this._owningObject._over = true;
        if(this._owningObject._onmouseover != null) this._owningObject._onmouseover();
        this._owningObject.__updatestyle();
      };
      
      /*private*/ this.__mouseout = function()
      { 
        ui.controllerHovered = false;
        this._owningObject._over = false;
        if(this._owningObject._onmouseout != null) this._owningObject._onmouseout();
        this._owningObject.__updatestyle();
      };
      
      /*private*/ this.__click = function()
      { 
        keymanweb['activatingUI'](false); // Clear activating UI flag once click is acknowledged
        if(this._owningObject._onclick != null) 
            return this._owningObject._onclick();
        return false;
      };
    
      /*private*/ this.__mousedown = function()
      {
        keymanweb['activatingUI'](true);  // Set activating UI flag (to manage focus/blur) on any UI mouse down event
        this._owningObject._down = true;
        this._owningObject.__updatestyle();
        return false;
      };
      
      /*private*/ this.__mouseup = function()
      {
        this._owningObject._down = false;
        this._owningObject.__updatestyle();
      };
      
      /*public*/ this._setSelected = function(_value)
      {
        keymanweb['activatingUI'](false); // Always clear activating UI flag after selecting UI
        this._owningObject._selected = _value;
        this._owningObject.__updatestyle();
      };
      
      /*public*/ this._getSelected = function()
      {
        return this._owningObject._selected;
      };
    
      /*public*/ this._getOver = function()
      {
        return this._owningObject._over;
      };
      
      /*public*/ this._getDown = function()
      {
        return this._owningObject._down;
      };
    
      this._owningObject = this; // simplifies meaning of 'this'
      
      var imgPath=util['getOption']('resources') + 'ui/toggle/';
      var _elemImg = util['createElement']('img');
      this._elem = util['createElement']('div');
      this._elem._owningObject = this;
      _elemImg.style.display = 'block';
      _elemImg.src = imgPath + _src;
      _elemImg.id = 'KMW_Controller_Img';
      this._elem.style.margin = '0px'; //display = 'inline';
      this._elem.style.width = '24px';
      this._elem.style.height = '24px';
      this._elem.style.zIndex = '10002';
      this._elem.style.lineHeight = '100%';
      this._elem.style.styleFloat = this._elem.style.cssFloat = 'left';
    
      _elemImg.title = _caption;
      _elemImg.alt = _caption;
      this._elem.appendChild(_elemImg);
      this._elem.onmouseover = this.__mouseover;
      this._elem.onmouseout = this.__mouseout;
      this._elem.onmousedown = this.__mousedown;
      this._elem.onmouseup = this.__mouseup;
      _elemImg._owningObject = this;
      _elemImg.onclick = this.__click;
      
      this.__updatestyle();
      
      return this;
    };
    
    /**
     * Function     Initialize
     * Scope        Private   
     * Description  Initialize Toggle User Interface
     **/   
    ui['initialize'] = ui.Initialize = function()
    { 
      //Never initialize before KMW!
      if(!keymanweb['initialized'] || util['isTouchDevice']()) return;
        
      if(!ui.initialized)  // I2403 - Allow toggle design to be loaded twice
      {
        ui.controller = util['createElement']('div');
      }
      else
        ui.controller.innerHTML = '';  // I2403 - Allow toggle design to be loaded twice
      
      var imgPath = util['getOption']('resources')+'ui/toggle/';	
      ui.controller.style.background = 'url('+imgPath+'kmwcontroller2x.gif)';    
      ui.controller.style.padding = '1px 2px';

      // Create keyboard list and OSK control buttones, and set initial styles
      var v1=util['loadCookie']('KeymanWeb_Keyboard'),kbdEnabledOnLoad=false;
      if(typeof(v1['current'])!='undefined') kbdEnabledOnLoad = (v1['current'].indexOf('---') < 0);
      ui.kbdButton = new ui.button('kmw_logo_16.gif', 'Use Web Keyboard', kbdEnabledOnLoad); 
      ui.controller.appendChild(ui.kbdButton.getElem());
    
      var v2 = util['loadCookie']('KeymanWeb_OnScreenKeyboard'),oskEnabledOnLoad=true; 
      if(typeof(v2['visible'])!='undefined') oskEnabledOnLoad=(v2['visible'] == 1); 
    
      // Add keyboard icon
      ui.oskButton = new ui.button('kmw_osk_16.gif','Show On Screen Keyboard',oskEnabledOnLoad); 
      ui.oskButton._onclick = ui.switchOsk;
      ui.controller.appendChild(ui.oskButton.getElem());

      // Hide controller unless already initialized
      if(!ui.initialized) ui.controller.style.display = 'none';
      ui.controller.style.zIndex = '10001';
      ui.controller.style.position = 'absolute';
      
      // The following three lines prevent the UI from being positioned incorrectly when the page is resized,
      // but don't fix the problem completely, as the kbd icon still moves.  probably need to insert a DIV
      // between the button and the container, and make that DIV fixed height and overflow:hidden 
      //ui.controller.style.maxHeight = '26px';  
      //ui.oskButton.getElem().style.position = 'relative';  
      //ui.oskButton.getElem().style.overflow = 'hidden';	
      
      if(!ui.initialized)  // I2403 - Allow toggle design to be loaded more than once if necessary
        document.body.appendChild(ui.controller);	   
    
      // Set initialized true  
      ui.initialized = true;  // I2403 - Allow toggle design to be loaded more than once if needed

      // Then update the keyboard list if keyboards already loaded (i.e. in page script)
      ui.updateKeyboardList();     
      
    }

    ui.shutdown = function() {
      var root = ui.controller;

      if(root) {
        root.parentNode.removeChild(root);
      }
    }

    /**
     * Function     updateKeyboardList
     * Scope        Private   
     * Description  Rebuild the UI and keyboard list
     **/   
    ui.updateKeyboardList=function()
    {                 
      if(!(keymanweb['initialized'] || ui.initialized)) return; //TODO: may want to restart the timer??
                      
      ui.updateList = false;

      var _kbds=keymanweb['getKeyboards'](),imgPath=util['getOption']('resources') +'ui/toggle/';	   
  
      // Check the number of installed keyboards to determine whether or not we will have a dropdown
      if(_kbds.length > 1)
      {
        // Multiple keyboards
        var _kmw_ctrl_img=<HTMLImageElement> document.getElementById('KMW_Controller_Img')
        _kmw_ctrl_img.src = imgPath+'kmw_logo_16_down.gif';
        _kmw_ctrl_img.style.width = '100%';
            
        ui.controller.style.background = 'url('+imgPath+'kmwcontroller2x.gif)';
        
        ui.kbdButton.getElem().id = 'kmwico';
        ui.kbdButton.getElem().style.width = '36px';
        ui.kbdButton._onmouseover = function()
          {
            ui.keyboardMenu.className="sfhover";
          };
        ui.kbdButton._onmouseout = function()
          {
            ui.keyboardMenu.className="sfunhover";
          };
        ui.kbdButton._onclick = null;
        ui.createMenu();
      }
      // Single keyboard
      else if(_kbds.length == 1)
      {
        var _kmw_ctrl_img=<HTMLImageElement> document.getElementById('KMW_Controller_Img')
        _kmw_ctrl_img.src = imgPath+'kmw_logo_16.gif';
        
        ui.kbdButton.getElem().id = 'kmwico';
        ui.kbdButton.getElem().style.width = '24px';

        var Lki=_kbds[0]['InternalName'];    
        var Lklc=_kbds[0]['LanguageCode'];  
        ui.controller.style.background = 'url('+imgPath+'kmwcontroller2.gif)';
        ui.keyboards.push({_InternalName: Lki, _LanguageCode: Lklc, _Index: 0});
        ui.kbdButton._onclick = ui.switchSingleKbd;
        ui.kbdButton._onmouseover = function() { };
        ui.kbdButton._onmouseout = function() { };

        // We must reconstruct the ui.keyboards array, and this done by ui.createMenu.
        ui.createMenu();

        // Must remove menu if keyboards have been removed leaving only a single keyboard
        if(typeof(ui.keyboardMenu) != 'undefined') delete ui.keyboardMenu;
      }
      
      // Highlight the last active keyboard
      var sk=keymanweb['getSavedKeyboard']().split(':');
      ui.updateMenu(sk[0],sk[1]);
    }  

    /**
     * Keyboard registration event handler
     *    
     * Set a timer to update the UI keyboard list on timeout after each keyboard is registered, 
     * thus updating only once when only if multiple keyboards are registered together
     */   
    keymanweb['addEventListener']('keyboardregistered', 
      function(p)
      {   
          ui.updateList = true;
          if(ui.updateTimer) clearTimeout(ui.updateTimer);
          ui.updateTimer = setTimeout(ui.updateKeyboardList,200);
      });       


    /**
     * Keyboard change event handler
     *        
     * Update menu selection and control OSK display appropriately
     */
    keymanweb['addEventListener']('keyboardchange',  
      function(p)
      {
        ui.updateMenu(p['internalName'],p['languageCode']);  
      });
    
    /* ----------------------------------------
      Drop down menu
      ---------------------------------------- */
    
  //  var  _SelectedMenuItem;
    
    /**
     * Function     selecKbd
     * Scope        Private 
     * @param       {number}  _kbd         
     * Description  Select a keyboard from the drop down menu 
     **/
    ui.selectKbd = function(_kbd)
    {
        var _name,_lgCode,_index;
        if(_kbd < 0) 
        { 
          _name = ''; _lgCode='';_index = ''; 
        }
        else 
        { 
          _name = ui.keyboards[_kbd]._InternalName; _lgCode = ui.keyboards[_kbd]._LanguageCode; _index = ui.keyboards[_kbd]._Index; 
        }
        keymanweb['setActiveKeyboard'](_name,_lgCode);
        keymanweb['focusLastActiveElement']();
        ui.kbdButton._setSelected(_name != '');
        if(_kbd >= 0) ui.lastActiveKeyboard = _kbd;

        return false;
    };
    
    /**
     * Function     updateMenu
     * Scope        Private
     * @param       {string}    kbdName
     * @param       {?string=}  lgCode
     * Description  Updates the menu selection when a change is required
     **/    
    ui.updateMenu = function(kbdName,lgCode)
    {        
      var i,_k=document.getElementById('KMWSel_$');

      for(i=0; i<ui.keyboards.length; i++)
      { 
        if(ui.keyboards[i]._InternalName == kbdName && ui.keyboards[i]._LanguageCode == lgCode)
          _k=document.getElementById('KMWSel_'+ui.keyboards[i]._InternalName+'$'+ui.keyboards[i]._Index);
      }
    
      if(_k)
      {
        if(ui.selectedMenuItem != null) ui.selectedMenuItem.className='';
        _k.className='selected'; ui.selectedMenuItem=_k;
      }

      // Occurs for desktop form-factors when no keyboard (aka the sys default) is active.
      if(!ui.oskButton) {
        return;
      }

      // Hide the OSK button for CJK keyboards (or non-mapped)
      if(lgCode=='cmn' || lgCode=='jpn' || lgCode=='kor')
      {
        ui.oskButton.getElem().style.display='none';
        //osk['show'](true);
      }
      else if(kbdName == '')
      {
        ui.oskButton.getElem().style.display='none';
      }
      else
      {
        ui.oskButton.getElem().style.display='block';
        //osk['show'](osk['isEnabled']());
      }
    }
    util['addStyleSheet'](
      "#KeymanWeb_KbdList {"+
        "display: block;"+
        "position: absolute;"+
        "width: auto;"+ ((util['getIEVersion']() < 8) ? "200px;" : "auto;")+
        "line-height: 100%;"+
        "margin: 0;"+
        "clear: both;"+
        "float: none;"+
        "top: auto;"+
        "border: solid 2px #ad4a28;"+
        "-moz-border-radius: 4px;"+
        "-webkit-border-radius: 4px;"+
        "border-radius: 4px;"+
        "box-shadow: 4px 4px 2px rgba(136,136,136,.5);"+
        "-webkit-box-shadow: 4px 4px 2px rgba(136,136,136,.5);"+
        "-moz-box-shadow: 4px 4px 2px rgba(136,136,136,.5);"+
        ((util['getIEVersion']() < 99) ? "filter:progid:DXImageTransform.Microsoft.DropShadow(OffX=4,OffY=4,Color=#80646464);" : "")+
        "list-style: none;"+
        "padding: 0;"+
        "background: white;"+
        "max-height: 300px;"+
        "overflow-y: scroll;"+ 
        "overflow-x: hidden;"+
        "white-space: nowrap;"+
        "z-index: 10001; /* above the osk */"+
      "}"+
      ((util['getIEVersion']() < 7) ? 
        "* html #KeymanWeb_KbdList {height: expression(this.scrollHeight > 299 ? '300px' : 'auto');}" : "")+    
      ".sfunhover#KeymanWeb_KbdList {"+
        "display: none; left: -999px;"+
      "}"+
      ".sfhover#KeymanWeb_KbdList {"+
        "display: block;"+
        "left: auto;"+
      "}"+
      "#KeymanWeb_KbdList li {"+
        "float: none;"+
        "width: auto;"+
        "padding: 0;"+
        "margin: 0;"+
      "text-align: left;"+
      "}"+
      "#KeymanWeb_KbdList li a {"+
        "display: block; "+
        "padding: 2px 4px;"+
        "color: #404040;"+
        "font-family: Tahoma,Verdana,Arial,sans-serif;"+
        "font-size: 8pt;"+
        "text-decoration: none;"+
      "}"+
      "#KeymanWeb_KbdList li a.selected {"+
        "font-weight: bold;"+
        "color: black;"+
      "}"+
      "#KeymanWeb_KbdList li a:hover {"+
        "color: white;"+
        "background-color: #ad4a28;"+
        "text-decoration: underline;"+
      "}");
    
    /**
     * Function     createMenu
     * Scope        Private   
     * Description  Create the drop down menu and populate with loaded KeymanWeb keyboards 
     **/
    ui.createMenu = function()
    {    
      if(typeof(ui.keyboardMenu) == 'undefined')  // I2403 - Allow toggle design to be loaded twice
      {
        ui.keyboardMenu=util['createElement']('ul');
        ui.keyboardMenu.id='KeymanWeb_KbdList';
        ui.keyboardMenu.className='sfunhover';
      }
      else ui.keyboardMenu.innerHTML = '';  // I2403 - Allow toggle design to be loaded twice
      
      var _li=util['createElement']('li');
      var _a=util['createElement']('a');
      _a.innerHTML='(System keyboard)';
      _a.href="#";
      _a.onclick = function() { return ui.selectKbd(-1); };
      _a.id='KMWSel_$';
      _a.className='selected';
      _li.appendChild(_a);
      
      ui.selectedMenuItem=_a;
      ui.keyboardMenu.appendChild(_li);
        
      var _kbds=keymanweb['getKeyboards'](), _added=[];
      ui.keyboards=[];
      for(var _kbd = 0; _kbd < _kbds.length; _kbd++)
      {
          var _li1=util['createElement']('li');
          var _a1=util['createElement']('a');
          _a1.innerHTML=_kbds[_kbd]['LanguageName'] + ' - ' + _kbds[_kbd]['Name'];
          if(!_added[_kbds[_kbd]['InternalName']]) _added[_kbds[_kbd]['InternalName']]=0;
          _added[_kbds[_kbd]['InternalName']]++;
    
          var _n=_added[_kbds[_kbd]['InternalName']];
          ui.keyboards.push({_InternalName: _kbds[_kbd]['InternalName'], _LanguageCode:_kbds[_kbd]['LanguageCode'], _Index: _n});

          _a1.href="#";
          _a1.onclick = (function(x) { return function() { return ui.selectKbd(x); } })(ui.keyboards.length-1);
          _a1.id='KMWSel_'+_kbds[_kbd]['InternalName']+'$'+_n;
    
          _li1.appendChild(_a1);
          ui.keyboardMenu.appendChild(_li1);
      }
      
      //if(!ui.initialized) // I2403 - Allow toggle design to be loaded twice
      if(ui.keyboardMenu.parentNode != ui.kbdButton.getElem()) ui.kbdButton.getElem().appendChild(ui.keyboardMenu);  
    };
    
    keymanweb['addHotKey'](191,0x20,ui.switchSingleKbd);
    keymanweb['addHotKey'](191,0x30,ui.switchNextKbd);
    keymanweb['addHotKey'](191,0x40,ui.switchOsk);

    // Initialize after KMW is fully initialized
    keymanweb['addEventListener']('loaduserinterface',ui.Initialize);
    
    // but also execute here, for asynchronous UI script loading (occurring after KMW initialization)
    ui.Initialize();
      
  } catch(ex){}
}