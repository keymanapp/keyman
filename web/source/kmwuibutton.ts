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
  /* Button User Interface Code   */
  /*                              */
  /********************************/

  /**
   * Do not enclose in an anonymous function, as the compiler may create
   * global scope variables to replace true, false, null, whcih can then collide
   * with other variables.
   * Instead, use the --output-wrapper command during optimization, which will
   * add the anonymous function to enclose all code, including those optimized
   * variables which would otherwise have global scope.
   **/  

  try {
    // Declare KeymanWeb, OnScreen keyboard and Util objects
    var keymanweb=window['keyman'],osk=keymanweb['osk'],
      util=keymanweb['util'],dbg=keymanweb['debug'];
    
    // Disable UI for touch devices
    if(util['isTouchDevice']()) throw '';
    
    // User interface global and variables      
    keymanweb['ui'] = {};
    var ui=keymanweb['ui'];
    ui['name'] = 'button';
  
    ui.init = false;  
    ui.KeyboardSelector = null;

    ui.KeymanWeb_DefaultKeyboardHelp='<span style="font-size:7.5pt">KeymanWeb is not running.  Choose a keyboard from the list</span>';
    ui._KeymanWeb_KbdList = null; 
    ui._KMWSel = null; 
    ui._IsHelpVisible = false; 
    ui._DefaultKeyboardID = '';

    ui.updateTimer = null;
    ui.updateList = true;
    
    /**
     * Highlight the currently active keyboard in the list of keyboards
     **/   
    ui._ShowSelected = function()
    {
      var _rv,kbd=keymanweb['getActiveKeyboard'](),lgc=keymanweb['getActiveLanguage'](), 
        kList = ui._KeymanWeb_KbdList.childNodes,
        _r = /^KMWSel_(.*)\$(.*)$/;
  
      for(var i=1; i<kList.length; i++)
        kList[i].childNodes[0].className = '';
        
      for(var i=2; i<kList.length; i++)
      {
        _rv = _r.exec(kList[i].childNodes[0].id);
        if(_rv && (_rv[1] == kbd) && (_rv[2] == lgc)) break;
      } 
      if(i >= kList.length) i=1;      
      kList[i].childNodes[0].className = 'selected'; 
    } 
      
    /**
     * Select keyboard by id
     * 
     * @param       {Event}  _id   keyboard selection event
     * @return      {boolean}   
     */
    ui._SelectKeyboard = function(_id)
    { 
      if(typeof(_id) == 'object') 
      {
        var t=null;
        if((typeof(_id.target) != 'undefined') && _id.target) t=_id.target;
        else if((typeof(_id.srcElement) != 'undefined') && _id.srcElement) t=_id.srcElement; 
        if(t) _id=t.id;
      }
      
      var _r=/^KMWSel_(.*)\$(.*)$/; 
      var _rv=_r.exec(_id),_lgc='',_name='';
      if(_rv !== null) 
      {
        _name = _rv[1].split('$')[0]; //new code
        _lgc = _id.split('$')[1];  
        if(ui._KMWSel != null) ui._KMWSel.className = '';
        var _k = document.getElementById(_id); 
        if(_k) _k.className='selected'; 
        ui._KMWSel = _k;  
        keymanweb['setActiveKeyboard'](_name,_lgc);
      }
      else 
        _name=null;
      
      keymanweb['focusLastActiveElement']();
      if(osk['isEnabled']()) osk['show'](true);    

      ui._ShowKeyboardButton(_name);
      return false;
    } 

    /**
     * Set KMW UI activation state on mouse click
     * 
     * @param       {Event}    e     event
     */    
    ui._SelectorMouseDown = function(e)
    { 
      var x=keymanweb['getLastActiveElement'](); 

      // Set the focus to an input field, to get correct OSK display behaviour
      if(!x) ui._FocusFirstInput(); else keymanweb['focusLastActiveElement']();
  
      if(keymanweb['activatingUI']) keymanweb['activatingUI'](1);
    }

    /**
     * Set focus on mouse up
     * 
     * @param       {Event}    e     event
     */    
    ui._SelectorMouseUp = function(e)
    { 
      var x=keymanweb['getLastActiveElement']();
      
      // Set the focus to an input field, to get correct OSK display behaviour
      if(!x) ui._FocusFirstInput(); else keymanweb['focusLastActiveElement']();
    }

    /**
     * Set KMW UI activation state on mouse over
     * 
     * @param       {Event}   e     event
     */    
    ui._SelectorMouseOver = function(e)
    {
      // highlight the currently active keyboard
      ui._ShowSelected();

      if(keymanweb['activatingUI']) keymanweb['activatingUI'](1);
      document.getElementById("kmwico_li").className="sfhover";

      // Conditionally display keyboard button
      ui._ShowKeyboardButton();
    }

    /**
     * Sets the focus to the first input or textarea found on the current page
     * to ensure consistent keyboard selection and OSK display behaviour   
     */       
    ui._FocusFirstInput = function()
    {
      var i,ip=null,tp=null,
        iList=document.getElementsByTagName("input"),
        tList=document.getElementsByTagName("textarea");
      
      for(i=0; i<iList.length; i++)
        if(iList[i].type == 'text') break; 
      
      if(i < iList.length) ip=iList[i];
      
      if(tList.length > 0) tp = tList[0];
      
      if((!ip) && (!tp))
        return;
      else if(ip && !tp)
        ip.focus();
      else if(tp && !ip)
        tp.focus();
      else if(ip.offsetTop < tp.offsetTop)
        ip.focus();
      else if(ip.offsetTop > tp.offsetTop)
        tp.focus();
      else if(ip.offsetLeft < tp.offsetLeft)
        ip.focus();
      else
        tp.focus();
    }
    
    /**
     * Clear KMW UI activation state on mouse out
     * 
     * @param       {Event}    e     event
     */    
    ui._SelectorMouseOut = function(e)
    {
      if(keymanweb['activatingUI']) keymanweb['activatingUI'](0);
      document.getElementById("kmwico_li").className="sfunhover";
    }

    /**
     * Disable the button to show/hide the OSK if no active keyboard or active keyboard is CJK (user cannot hide)
     * 
     * @param       {?string=}  _name     current keyboard name
     */    
    ui._ShowKeyboardButton = function(_name)
    {
      var kbdName = keymanweb['getActiveKeyboard'](), kbdId=document.getElementById("KMW_Keyboard");
      if(arguments.length > 0) kbdName = _name;
      if(kbdId)
      {
        if((kbdName == '') || keymanweb['isCJK']())
        { 
          kbdId.className='kmw_disabled';
        } 
        else
        {
          kbdId.className = osk['isEnabled']() ? 'kmw_show' : 'kmw_hide';
        }
      }
    }  
  
    /**
     * UI Functions called by KeymanWeb or OSK
     */     
    osk['addEventListener']('show',
      function(oskPosition)
      { 
        var t=keymanweb['getLastActiveElement']();
        if(t)
        {  
          if(!oskPosition['userLocated'])
          {
            oskPosition['x'] = util['getAbsoluteX'](t);
            oskPosition['y'] = util['getAbsoluteY'](t)+t.offsetHeight;
          }
        } 
        
        ui._ShowKeyboardButton();
        return oskPosition; 
      }); 
  /* TODO: why is this still needed??? Does it actually do anything?? */ 
    osk['addEventListener']('hide',
      function(hiddenByUser)
      { 
        if((arguments.length > 0) && hiddenByUser)
        {
          var _a = document.getElementById('KMW_Keyboard');
          if(_a) _a.className = 'kmw_hide';
        }    
      }); 
    
    /**
     * Show or hide the OSK (always visible for CJK keyboards)
     * 
     * @param       {Object}    _anchor     anchor element (?)
     * @return      {boolean}   
     **/   
    ui._ShowKeymanWebKeyboard = function(_anchor) 
    { 
      var kbdId=document.getElementById("KMW_Keyboard");
      if((kbdId.className!='kmw_disabled') && osk['show']) 
      {
        if(osk['isEnabled']()) osk['hide'](); else osk['show'](true);
      }
      if(window.event) window.event.returnValue=false;

      keymanweb['focusLastActiveElement']();    
      return false;    
    }

    /**
     * Initialize Button User Interface
     **/   
    ui.Initialize = function()
    {
      //Never initialize UI before KMW (parameters will be undefined)
      if(!keymanweb['initialized'])
      {
        window.setTimeout(ui.Initialize,250); return;
      }
      
      if(ui.init || util['isTouchDevice']()) return; 
      
      ui.init = true;
          
      util['addStyleSheet'](ui._Appearance);
    
      ui._KeymanWeb_KbdList = util['createElement']('ul');
      ui._KeymanWeb_KbdList.id = 'KeymanWeb_KbdList';
      
      var _elem = document.getElementById('KeymanWebControl');
      if(!_elem)
      {
        var _elems = document.getElementsByTagName('div');
        for(var _i = 0; _i < _elems.length; _i++)
        {
          if(_elems[_i].className == 'KeymanWebControl')
          {
            _elem = _elems[_i]; break;
          }
        }
      }
      
      // Insert as first child of body if not defined by user
      if(!_elem && (document.body != null))
      {
        _elem=document.createElement('DIV');
        _elem.id='KeymanWebControl';  
        document.body.insertBefore(_elem,document.body.firstChild);
        ui._insertedElem = _elem;
      }
    
    
      var imgPath=util['getOption']('resources')+'ui/button/';
      if(_elem)
      { 
        // Append another DIV to follow the main control with clear:both to prevent selection over entire width of window
        var dx=document.createElement('DIV'),ds=dx.style;
        ds.clear='both';
        _elem.parentNode.insertBefore(dx,_elem.nextSibling);
      
        var _btn=util['createElement']('img'), _ul=util['createElement']('ul'),_li0=util['createElement']('li');
        _btn.id = 'kmwico_a';
        _btn.src = imgPath+'kmw_button.gif';
        _btn.onclick = function(){return false;}  //may want to use this in iOS *****
        _li0.appendChild(_btn);
        _li0.id = 'kmwico_li';
        _ul.appendChild(_li0);
        _ul.id = 'kmwico';
        _ul.style.display = 'block';
        _elem.appendChild(_ul);
      }
      // Do not define any UI behaviour if no controller element can be found
      else return;

      if(!keymanweb['iOS'])
      {   
        var _li = util['createElement']('li'); 
        var _a = util['createElement']('a');
        var _img = util['createElement']('img');
        _img.src = imgPath+'kbdicon.gif';
        _a.appendChild(_img);
        
        var _txt1 = document.createTextNode(' Hide Keyboard');    
        var _txt2 = document.createTextNode(' Show Keyboard');
        var _sp1 = util['createElement']('span');
        _sp1.id = 'KMW_KbdVisibleMsg';
        _sp1.appendChild(_txt1);
        _a.appendChild(_sp1);
        
        var _sp2 = util['createElement']('span');
        _sp2.id = 'KMW_KbdHiddenMsg';
        _sp2.appendChild(_txt2);
        _a.appendChild(_sp2);
        _a.onmousedown = ui._ShowKeymanWebKeyboard;
        _a.href = '#';
        _a.id = 'KMW_Keyboard';
        _li.id = 'KMW_ButtonUI_KbdIcon';
        _li.appendChild(_a);
        ui._KMWSel = _a;
        ui._KeymanWeb_KbdList.appendChild(_li);
      }

      var _li1 = util['createElement']('li'); 
      _li1.id = 'KMW_ButtonUI_KbdList'; 
      var _a1 = util['createElement']('a');
      _a1.appendChild(document.createTextNode('(System keyboard)'));

      _a1.onclick = ui._SelectKeyboard;
      _a1.href = '#'; 
      _a1.id='KMWSel_$';
      _a1.className='selected';
      _li1.appendChild(_a1);
      ui._KMWSel = _a1;
      ui._KeymanWeb_KbdList.appendChild(_li1);

      var _kbds = keymanweb['getKeyboards'](), _added = []; 

      ui.updateKeyboardList();
          
      document.getElementById('kmwico_li').appendChild(ui._KeymanWeb_KbdList);

      var _sfEl = document.getElementById("kmwico_li");
      util['attachDOMEvent'](_sfEl,'mousedown',ui._SelectorMouseDown);
      util['attachDOMEvent'](_sfEl,'mouseover',ui._SelectorMouseOver);
      util['attachDOMEvent'](_sfEl,'mouseout',ui._SelectorMouseOut);    
      util['attachDOMEvent'](_sfEl,'mouseup',ui._SelectorMouseUp);
      
      keymanweb['focusLastActiveElement']();  	//TODO: this needs to be extended - if no element is active, try and identify an enabled input element
    }

    ui.shutdown = function() {
      var root = ui._insertedElem;
      if(root) {
        root.parentNode.removeChild(root);
      }
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
          ui.updateTimer = setTimeout(ui.updateKeyboardList,20);
      });       


    /**
     * Update the entire menu when keyboards are registered or deregistered
     **/   
    ui.updateKeyboardList = function()
    {  
      ui.updateList = false;
      
      if(!ui.init) return;
  
      // Clear existing list first (first two nodes must be preserved)
      for(var i:number=ui._KeymanWeb_KbdList.childNodes.length; i>2; i--)
        ui._KeymanWeb_KbdList.removeChild(ui._KeymanWeb_KbdList.childNodes[i-1]);
      
      var kbds=keymanweb['getKeyboards']();
      if(kbds.length > 0)
      {
        for(var i:number=0; i<kbds.length; i++)
          ui.registerKeyboard(kbds[i]['InternalName'],kbds[i]['LanguageName'],kbds[i]['Name'],kbds[i]['LanguageCode'],kbds[i]['RegionCode']);
      }
    } 

    /**
     * As each keyboard stub is registered, append it to the list
     * 
     * @param       {string}    Lki     internal name
     * @param       {string}    Lkl     language name
     * @param       {string}    Lkn     keyboard name
     * @param       {string}    Lklc    language code
     * @param       {string}    Lkrc    region code
     **/   
    ui.registerKeyboard = function(Lki,Lkl,Lkn,Lklc,Lkrc)
    {    
      var _li2 = util['createElement']('li'),
        _a2 = util['createElement']('a'),
        _t = Lkn.replace(/\s?keyboard/i,''); 
  
      if(Lkl)
      {
        var lg=Lkl.split(',')[0];
        if(Lkn.search(lg) == -1)
          _t = lg+' ('+_t+')';
      }   
      if(_t.length > 26) _t=_t.substr(0,24)+'\u2026';
      _a2.appendChild(document.createTextNode(_t));      
      _a2.onclick = ui._SelectKeyboard;
      _a2.href = '#';
      _a2.id='KMWSel_'+Lki+'$'+Lklc;
      _li2.appendChild(_a2);
      ui._KeymanWeb_KbdList.appendChild(_li2);
    }
    
    // Define appearance of this interface
    ui._Appearance = 
      "#kmwico, #kmwkbd {"+
        "vertical-align: middle;"+
      "} "+

      "#KeymanWebControl {float:left;} "+
      
      "#KeymanWebControl * "+
      "{"+
        "letter-spacing: 0px !important;"+
        "line-height: 1li !important;"+
        "white-space: nowrap !important;"+
      "} "+
                
      "#KeymanWebControl #kmwico img {"+
        "vertical-align: top;"+
        "padding: 0;"+
        "margin: 0;"+
        "border: none;"+
      "} "+
              
      "#KeymanWebControl #kmwico, #kmwico ul {"+
        "padding: 0;"+
        "margin: 0;"+
        "list-style: none;"+
      "} "+
              
      "#KeymanWebControl #kmwico_a {"+
        "display: block;"+
      //"border: none !important;"+
        "width: 22px; height: 23px; "+                                 /* sizes needed for kmw_button.gif */
      "} "+
              
      "#KeymanWebControl #kmwico li { "+
        "text-align: left;"+
      "} "+
    
      "#KeymanWebControl #kmwico li ul {"+
        "display: block;"+
        "position: absolute;"+
        "left: -5999px;"+
        "border: solid 2px #ad4a28;"+
        "background: white;"+
        "border-radius: 4px;"+
        "box-shadow: 4px 4px 2px #666;"+
        "z-index: 10011;"+ /* above the osk */
      "} "+
              
      "#KeymanWebControl #kmwico li.sfunhover ul {"+
        "display: none; left: -5999px;"+
      "} "+
              
      "#KeymanWebControl #kmwico li:hover ul, #kmwico li.sfhover ul {"+
        "display: block;"+
        "left: auto;"+
      "} "+
    
      "#KeymanWebControl #kmwico ul li {"+
        "float: none;"+
        "padding: 0 !important;"+
        "margin: 0 !important;"+
        "width: 136px !important;"+
      "} "+
    
      "#KeymanWebControl #KMW_LanguageName {"+
        "font-weight: bold;"+
      "} "+
    
      "#KeymanWebControl #kmwico ul li a, #kmwico ul li a:visited {"+
        "display: block;"+
        "padding: 2px 4px !important;"+
        "border: none !important;"+
      //  "width: auto;"+
        "color: #404040;"+
        "font-family: Tahoma,Verdana,Arial,sans-serif;"+
        "font-size: 8pt;"+
        "text-decoration: none;"+
      "} "+
    
      "#KeymanWebControl #kmwico ul li a.selected {"+
        "font-weight: bold;"+
        "color: black;"+
      "} "+
    
      "#KeymanWebControl #kmwico ul li a:hover {"+
        "color: white;"+
        "background-color: #ad4a28;"+
      "} "+
              
      "#KeymanWebControl #kmwico ul li a.kmw_disabled, #KeymanWebControl #kmwico ul li a.kmw_disabled:hover {"+
        "color: #c0c0c0; cursor: default;"+
        "background-color: white;"+
      "} "+

      "#KeymanWebControl #kmwico ul li a.kmw_show span#KMW_KbdHiddenMsg, #KeymanWebControl #kmwico ul li a.kmw_disabled span#KMW_KbdVisibleMsg {"+
        "display: none;"+
      "} "+

      "#KeymanWebControl #kmwico ul li a.kmw_show span#KMW_KbdVisibleMsg {"+
        "display: inline;"+
      "} "+

      "#KeymanWebControl #kmwico ul li a.kmw_hide span#KMW_KbdHiddenMsg {"+
        "display: inline;"+
      "} "+

      "#KeymanWebControl #kmwico ul li a.kmw_hide span#KMW_KbdVisibleMsg {"+
        "display: none;"+
      "} "+

      "#KeymanWebControl #kmwico ul li#KMW_ButtonUI_KbdIcon {"+
        "border-bottom: solid 1px #ad4a28;"+
      "} ";   

    // Initialize after KMW is fully initialized, if UI already loaded
    keymanweb['addEventListener']('loaduserinterface',ui.Initialize);
    
    // but also call initialization when script loaded, which is after KMW initialization for asynchronous script loading
    ui.Initialize();
      
  } catch(err){}
}