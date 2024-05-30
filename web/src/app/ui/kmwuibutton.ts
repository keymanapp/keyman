/***
   KeymanWeb 17.0
   Copyright 2019-2023 SIL International
***/

import type { KeymanEngine, UIModule } from 'keyman/app/browser';

declare var keyman: KeymanEngine

if(!keyman?.ui?.name) {

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
    const keymanweb = keyman;
    const util=keymanweb.util;
    // var dbg=keymanweb['debug'];

    // Disable UI for touch devices
    if(util.isTouchDevice()) {
      throw '';
    }

    class UIButton implements UIModule {
      // User interface global and variables
      readonly name = 'button';

      init = false;

      /**
       * Initialization timeout handle.
       */
      private initTimer: number = null;

      private _KeymanWeb_KbdList: HTMLUListElement = null;

      /**
       * Tracks the element corresponding to the active keyboard on the keyboard list.
       *
       * Is also set when activating it due to user interaction.
       */
      private _KMWSel: HTMLElement = null;

      updateTimer: number = 0;
      updateList = true;

      /**
       * If no id == 'KeymanWebControl' element exists during init, this UI module
       * will make its own, auto-insert it, and track the element here.
       */
      private _insertedElem: HTMLElement = null;

      /**
       * Highlight the currently active keyboard in the list of keyboards
       **/
      private _ShowSelected() {
        let kbd=keymanweb.getActiveKeyboard();
        let lgc=keymanweb.getActiveLanguage();
        let kList = this._KeymanWeb_KbdList.childNodes;
        let _r = /^KMWSel_(.*)\$(.*)$/;

        for(let i=1; i<kList.length; i++) {
          (kList[i].childNodes[0] as HTMLElement).className = '';
        }

        let i: number;
        for(i=2; i<kList.length; i++) {
          let _rv = _r.exec((kList[i].childNodes[0] as HTMLElement).id);
          if(_rv && (_rv[1] == kbd) && (_rv[2] == lgc)) {
            break;
          }
        }
        if(i >= kList.length) {
          i=1;
        }
        (kList[i].childNodes[0] as HTMLElement).className = 'selected';
      }

      /**
       * Select keyboard by id
       *
       * @param       {Event}  _id   keyboard selection event
       * @return      {boolean}
       */
      readonly _SelectKeyboard = (_id: Event) => {
        let id: string = '';
        if(typeof(_id) == 'object') {
          let t: HTMLElement = null;
          if((typeof(_id.target) != 'undefined') && _id.target) {
            t=_id.target as HTMLElement;
          } else if((typeof(_id.srcElement) != 'undefined') && _id.srcElement) {
            t=_id.srcElement as HTMLElement;
          }

          if(t) {
            id=t.id;
          }
        }

        let _r=/^KMWSel_(.*)\$(.*)$/;
        let _rv=_r.exec(id),_lgc='',_name='';
        if(_rv !== null) {
          _name = _rv[1].split('$')[0]; //new code
          _lgc = id.split('$')[1];
          if(this._KMWSel != null) {
            this._KMWSel.className = '';
          }
          let _k = document.getElementById(id);
          if(_k) {
            _k.className='selected';
          }
          this._KMWSel = _k;
          keymanweb.setActiveKeyboard(_name,_lgc);
        } else {
          _name=null;
        }

        keymanweb.focusLastActiveElement();
        let osk = keymanweb.osk;
        if(osk && osk.isEnabled()) {
          osk.show(true);
        }

        this._ShowKeyboardButton(_name);
        return false;
      }

      /**
       * Set KMW UI activation state on mouse click
       *
       * @param       {Event}    e     event
       */
      readonly _SelectorMouseDown = (e: MouseEvent) => {
        const x=keymanweb.getLastActiveElement();

        // Set the focus to an input field, to get correct OSK display behaviour
        if(!x) {
          this._FocusFirstInput();
        } else {
          keymanweb.focusLastActiveElement();
        }

        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(1);
        }
      }

      /**
       * Set focus on mouse up
       *
       * @param       {Event}    e     event
       */
      readonly _SelectorMouseUp = (e: MouseEvent) => {
        const x=keymanweb.getLastActiveElement();

        // Set the focus to an input field, to get correct OSK display behaviour
        if(!x) {
          this._FocusFirstInput();
        } else {
          keymanweb.focusLastActiveElement();
        }
      }

      /**
       * Set KMW UI activation state on mouse over
       *
       * @param       {Event}   e     event
       */
      readonly _SelectorMouseOver = (e: MouseEvent) => {
        // highlight the currently active keyboard
        this._ShowSelected();

        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(1);
        }

        document.getElementById("kmwico_li").className="sfhover";

        // Conditionally display keyboard button
        this._ShowKeyboardButton();
      }

      /**
       * Sets the focus to the first input or textarea found on the current page
       * to ensure consistent keyboard selection and OSK display behaviour
       */
      private _FocusFirstInput() {
        let ip: HTMLInputElement = null;
        let tp: HTMLTextAreaElement = null;
        const iList=document.getElementsByTagName("input");
        const tList=document.getElementsByTagName("textarea");

        let i: number;
        for(i=0; i<iList.length; i++) {
          if(iList[i].type == 'text') {
            break;
          }
        }

        if(i < iList.length) {
          ip=iList[i];
        }

        if(tList.length > 0) {
          tp = tList[0];
        }

        if((!ip) && (!tp)) {
          return;
        } else if(ip && !tp) {
          ip.focus();
        } else if(tp && !ip) {
          tp.focus();
        } else if(ip.offsetTop < tp.offsetTop) {
          ip.focus();
        } else if(ip.offsetTop > tp.offsetTop) {
          tp.focus();
        } else if(ip.offsetLeft < tp.offsetLeft) {
          ip.focus();
        } else {
          tp.focus();
        }
      }

      /**
       * Clear KMW UI activation state on mouse out
       *
       * @param       {Event}    e     event
       */
      readonly _SelectorMouseOut = (e: MouseEvent) => {
        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(0);
        }
        document.getElementById("kmwico_li").className="sfunhover";
      }

      /**
       * Disable the button to show/hide the OSK if no active keyboard or active keyboard is CJK (user cannot hide)
       *
       * @param       {?string=}  _name     current keyboard name
       */
      private _ShowKeyboardButton(_name?: string) {
        let kbdName = keymanweb.getActiveKeyboard();
        let kbdId = document.getElementById("KMW_Keyboard");
        if(arguments.length > 0) {
          kbdName = _name;
        }

        if(kbdId) {
          if((kbdName == '') || keymanweb.isCJK()) {
            kbdId.className='kmw_disabled';
          } else {
            let osk = keymanweb.osk;
            kbdId.className = osk && osk.isEnabled() ? 'kmw_show' : 'kmw_hide';
          }
        }
      }

      registerEvents() {
        let osk = keymanweb.osk;
        if(!osk) {
          return;
        }
        /**
         * UI Functions called by KeymanWeb or OSK
         */
        osk.addEventListener('show', (oskPosition) => {
          const t=keymanweb.getLastActiveElement();
          if(t) {
            if(!oskPosition['userLocated']) {
              oskPosition['x'] = util.getAbsoluteX(t);
              oskPosition['y'] = util.getAbsoluteY(t)+t.offsetHeight;
            }
          }

          this._ShowKeyboardButton();
          return oskPosition;
        });

        /* TODO: why is this still needed??? Does it actually do anything?? */
        osk.addEventListener('hide', function(obj) {
          if((arguments.length > 0) && obj['hiddenByUser']) {
            let _a = document.getElementById('KMW_Keyboard');
            if(_a) {
              _a.className = 'kmw_hide';
            }
          }
        });
      }

      /**
       * Show or hide the OSK (always visible for CJK keyboards)
       *
       * @param       {Object}    _anchor     anchor element (?)
       * @return      {boolean}
       **/
      readonly _ShowKeymanWebKeyboard = () => {
        const kbdId=document.getElementById("KMW_Keyboard");
        let osk = keymanweb.osk;

        if((kbdId.className!='kmw_disabled') && osk && osk.show) {
          if(osk.isEnabled()) {
            osk.hide();
          } else {
            osk.show(true);
          }
        }
        if(window.event) {
          window.event.returnValue=false;
        }

        keymanweb.focusLastActiveElement();
        return false;
      }

      /**
       * Initialize Button User Interface
       **/
      readonly initialize = () => {
        if(this.initTimer) {
          window.clearTimeout(this.initTimer);
          this.initTimer = null;
        }

        //Never initialize UI before KMW (parameters will be undefined)
        if(!keymanweb.initialized) {
          this.initTimer = window.setTimeout(this.initialize, 50);
          return;
        }

        if(this.init || util.isTouchDevice()) {
          return;
        }

        this.init = true;

        util.addStyleSheet(this._Appearance);

        this._KeymanWeb_KbdList = util.createElement('ul');
        this._KeymanWeb_KbdList.id = 'KeymanWeb_KbdList';

        let _elem = document.getElementById('KeymanWebControl');
        if(!_elem) {
          const _elems = document.getElementsByTagName('div');
          for(let _i = 0; _i < _elems.length; _i++) {
            if(_elems[_i].className == 'KeymanWebControl') {
              _elem = _elems[_i]; {
                break;
              }
            }
          }
        }

        // Insert as first child of body if not defined by user
        if(!_elem && (document.body != null)) {
          _elem=document.createElement('DIV');
          _elem.id='KeymanWebControl';
          document.body.insertBefore(_elem,document.body.firstChild);
          this._insertedElem = _elem;
        }


        let imgPath=util.getOption('resources')+'ui/button/';
        if(_elem) {
          // Append another DIV to follow the main control with clear:both to prevent selection over entire width of window
          const dx=document.createElement('DIV')
          const ds=dx.style;
          ds.clear='both';
          _elem.parentNode.insertBefore(dx,_elem.nextSibling);

          const _btn=util.createElement('img');
          const _ul=util.createElement('ul');
          const _li0=util.createElement('li');

          _btn.id = 'kmwico_a';
          _btn.src = imgPath+'kmw_button.gif';
          _btn.onclick = function() {
            return false;
          }  //may want to use this in iOS *****
          _li0.appendChild(_btn);
          _li0.id = 'kmwico_li';
          _ul.appendChild(_li0);
          _ul.id = 'kmwico';
          _ul.style.display = 'block';
          _elem.appendChild(_ul);
        } else {
          // Do not define any UI behaviour if no controller element can be found
          return;
        }

        // ... wait, what?  I can't find any evidence of this reference point existing.
        // Even tag `release-web-stable-2.0` turns up results only for this specific sourcefile.
        // Thus, in essence:  if(true) { /* ... */ }
        if(!keymanweb['iOS']) {
          var _li = util.createElement('li');
          var _a = util.createElement('a');
          var _img = util.createElement('img');
          _img.src = imgPath+'kbdicon.gif';
          _a.appendChild(_img);

          const _txt1 = document.createTextNode(' Hide Keyboard');
          const _txt2 = document.createTextNode(' Show Keyboard');
          const _sp1 = util.createElement('span');
          _sp1.id = 'KMW_KbdVisibleMsg';
          _sp1.appendChild(_txt1);
          _a.appendChild(_sp1);

          var _sp2 = util.createElement('span');
          _sp2.id = 'KMW_KbdHiddenMsg';
          _sp2.appendChild(_txt2);
          _a.appendChild(_sp2);
          _a.onmousedown = this._ShowKeymanWebKeyboard;
          _a.href = '#';
          _a.id = 'KMW_Keyboard';
          _li.id = 'KMW_ButtonUI_KbdIcon';
          _li.appendChild(_a);
          this._KMWSel = _a;
          this._KeymanWeb_KbdList.appendChild(_li);
        }

        var _li1 = util.createElement('li');
        _li1.id = 'KMW_ButtonUI_KbdList';
        var _a1 = util.createElement('a');
        _a1.appendChild(document.createTextNode('(System keyboard)'));

        _a1.onclick = this._SelectKeyboard;
        _a1.href = '#';
        _a1.id='KMWSel_$';
        _a1.className='selected';
        _li1.appendChild(_a1);
        this._KMWSel = _a1;
        this._KeymanWeb_KbdList.appendChild(_li1);

        this.updateKeyboardList();

        document.getElementById('kmwico_li').appendChild(this._KeymanWeb_KbdList);

        var _sfEl = document.getElementById("kmwico_li");
        util.attachDOMEvent(_sfEl,'mousedown',this._SelectorMouseDown);
        util.attachDOMEvent(_sfEl,'mouseover',this._SelectorMouseOver);
        util.attachDOMEvent(_sfEl,'mouseout',this._SelectorMouseOut);
        util.attachDOMEvent(_sfEl,'mouseup',this._SelectorMouseUp);

        this.registerEvents();
        keymanweb.focusLastActiveElement();  	//TODO: this needs to be extended - if no element is active, try and identify an enabled input element
      }

      shutdown() {
        const root = this._insertedElem;
        if(root) {
          root.parentNode.removeChild(root);
        }
      }

      /**
       * Update the entire menu when keyboards are registered or deregistered
       **/
      readonly updateKeyboardList = () => {
        this.updateList = false;

        if(!this.init) {
          return;
        }

        // Clear existing list first (first two nodes must be preserved)
        for(let i = this._KeymanWeb_KbdList.childNodes.length; i>2; i--) {
          this._KeymanWeb_KbdList.removeChild(this._KeymanWeb_KbdList.childNodes[i-1]);
        }

        const kbds=keymanweb.getKeyboards();
        if(kbds.length > 0) {
          for(var i:number=0; i<kbds.length; i++) {
            this.registerKeyboard(
              kbds[i].InternalName,
              kbds[i].LanguageName,
              kbds[i].Name,
              kbds[i].LanguageCode
            );
          }
        }
      }

      /**
       * As each keyboard stub is registered, append it to the list
       *
       * @param       {string}    Lki     internal name
       * @param       {string}    Lkl     language name
       * @param       {string}    Lkn     keyboard name
       * @param       {string}    Lklc    language code
       **/
      registerKeyboard(
        Lki: string,
        Lkl: string,
        Lkn: string,
        Lklc: string
      ) {
        const _li2 = util.createElement('li');
        const _a2 = util.createElement('a');
        let _t = Lkn.replace(/\s?keyboard/i,'');

        if(Lkl) {
          var lg=Lkl.split(',')[0];
          if(Lkn.search(lg) == -1) {
            _t = lg+' ('+_t+')';
          }
        }
        if(_t.length > 26) {
          _t=_t.substr(0,24)+'\u2026';
        }
        _a2.appendChild(document.createTextNode(_t));
        _a2.onclick = this._SelectKeyboard;
        _a2.href = '#';
        _a2.id='KMWSel_'+Lki+'$'+Lklc;
        _li2.appendChild(_a2);
        this._KeymanWeb_KbdList.appendChild(_li2);
      }

      // Define appearance of this interface
      private _Appearance = `
#kmwico, #kmwkbd {
  vertical-align: middle;
}

#KeymanWebControl {
  float:left;
}

#KeymanWebControl * {
  letter-spacing: 0px !important;
  line-height: 1li !important;
  white-space: nowrap !important;
}

#KeymanWebControl #kmwico img {
  vertical-align: top;
  padding: 0;
  margin: 0;
  border: none;
}

#KeymanWebControl #kmwico, #kmwico ul {
  padding: 0;
  margin: 0;
  list-style: none;
}

#KeymanWebControl #kmwico_a {
  display: block;
  /* border: none !important; */
  width: 22px; height: 23px;                                 /* sizes needed for kmw_button.gif */
}

#KeymanWebControl #kmwico li {
  text-align: left;
}

#KeymanWebControl #kmwico li ul {
  display: block;
  position: absolute;
  left: -5999px;
  border: solid 2px #ad4a28;
  background: white;
  border-radius: 4px;
  box-shadow: 4px 4px 2px #666;
  z-index: 10011; /* above the osk */
}

#KeymanWebControl #kmwico li.sfunhover ul {
  display: none; left: -5999px;
}

#KeymanWebControl #kmwico li:hover ul, #kmwico li.sfhover ul {
  display: block;
  left: auto;
}

#KeymanWebControl #kmwico ul li {
  float: none;
  padding: 0 !important;
  margin: 0 !important;
  width: 136px !important;
}

#KeymanWebControl #KMW_LanguageName {
  font-weight: bold;
}

#KeymanWebControl #kmwico ul li a, #kmwico ul li a:visited {
  display: block;
  padding: 2px 4px !important;
  border: none !important;
  /* width: auto; */
  color: #404040;
  font-family: Tahoma,Verdana,Arial,sans-serif;
  font-size: 8pt;
  text-decoration: none;
}

#KeymanWebControl #kmwico ul li a.selected {
  font-weight: bold;
  color: black;
}

#KeymanWebControl #kmwico ul li a:hover {
  color: white;
  background-color: #ad4a28;
}

#KeymanWebControl #kmwico ul li a.kmw_disabled, #KeymanWebControl #kmwico ul li a.kmw_disabled:hover {
  color: #c0c0c0; cursor: default;
  background-color: white;
}

#KeymanWebControl #kmwico ul li a.kmw_show span#KMW_KbdHiddenMsg, #KeymanWebControl #kmwico ul li a.kmw_disabled span#KMW_KbdVisibleMsg {
  display: none;
}

#KeymanWebControl #kmwico ul li a.kmw_show span#KMW_KbdVisibleMsg {
  display: inline;
}

#KeymanWebControl #kmwico ul li a.kmw_hide span#KMW_KbdHiddenMsg {
  display: inline;
}

#KeymanWebControl #kmwico ul li a.kmw_hide span#KMW_KbdVisibleMsg {
  display: none;
}

#KeymanWebControl #kmwico ul li#KMW_ButtonUI_KbdIcon {
  border-bottom: solid 1px #ad4a28;
}
`
    }

    const ui = keymanweb.ui = new UIButton();

    /**
     * Keyboard registration event handler
     *
     * Set a timer to update the UI keyboard list on timeout after each keyboard is registered,
     * thus updating only once when only if multiple keyboards are registered together
     */
    keymanweb.addEventListener('keyboardregistered', function(p) {
      ui.updateList = true;
      if(ui.updateTimer) {
        clearTimeout(ui.updateTimer);
      }
      ui.updateTimer = window.setTimeout(ui.updateKeyboardList, 20);
    });

    // but also call initialization when script loaded, which is after KMW initialization for asynchronous script loading
    ui.initialize();
  } catch(err){}
}