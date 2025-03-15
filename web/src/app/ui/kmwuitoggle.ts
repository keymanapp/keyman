/***
   KeymanWeb 17.0
   Copyright 2019-2023 SIL International
***/

import type { KeymanEngine, KeyboardCookie, UIModule } from 'keyman/app/browser';
import type { FloatingOSKViewCookie } from 'keyman/engine/osk';

declare global {
  interface Window {
    keyman: KeymanEngine
  }
}

type KeyboardMenuEntry = {
  _InternalName: string,
  _LanguageCode: string,
  _Index: number;
}

interface Owned<T> {
  _owningObject?: T;
}

const keymanweb=window.keyman;

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!keymanweb) {
  throw new Error("`keyman` global is missing; Keyman Engine for Web script has not been loaded");
} else if(!keymanweb.ui?.name) {
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
    //var dbg=keymanweb['debug'];
    const util = keymanweb.util;

    // Disable UI for touch devices
    if(util.isTouchDevice()) {
      throw '';
    }

    class ToggleUI implements UIModule {
      // Initialize user interface common variables
      readonly name = 'toggle';
      initialized = false;

      /**
       * The top-level element of the Toggle UI module.
       */
      controller: HTMLDivElement = null;
      oskButton: ReturnType<ToggleUI['button']> = null;  // inner class - Button, as defined within the .button method
      kbdButton: ReturnType<ToggleUI['button']> = null;  // likewise.
      controllerHovered = false;

      keyboards: KeyboardMenuEntry[] = [];
      /**
       * The index of the previously-active keyboard in this UI module's language list.
       */
      lastActiveKeyboard = -1;

      selectedMenuItem: HTMLAnchorElement = null;
      updateList = true;

      /**
       * The numeric handle for an active window.setTimeout call.
       */
      updateTimer: number = null;

      keyboardMenu: HTMLUListElement;

      /**
       * Update the KeymanWeb user interface when an input element is focused or blurred
       *
       * @param       {Object}            someElement     focused element
       * @param       {(boolean|number)}  focusing        true if focusing
       * @param       {Object}            activeControl   Object representing API specs for the control, if it exists and is now focused.
       */
      doFocus(someElement: HTMLElement, focusing: boolean | number, activeControl: HTMLElement) {
        // This callback must be ignored until UI is initialized, or for touch devices (which can never initialize the UI)
        if(!this.initialized) {
          return;
        }

        // We don't want to shift the controller to something that's not an input element,
        // but do want to account for window.event's data when legitimate.
        if(window.event && keymanweb.isAttached(window.event.srcElement as HTMLElement)) {
          someElement=window.event.srcElement as HTMLElement;
        }

        if(focusing) {
          this.controller.style.display = 'block';
        } else {
          if(!(keymanweb.getUIState().activationPending) && !this.controllerHovered) {
            this.controller.style.display = 'none';
          }
        }

        /* I2406 - Find an appropriate position for the controller */
        let w: number, h: number;

        const p = util.getAbsolute(someElement);
        let x = p.x;
        let y = p.y;

        var ownerDoc = someElement.ownerDocument;
        if(ownerDoc.designMode == 'on' && ownerDoc.defaultView && ownerDoc.defaultView.frameElement) {
          w = ownerDoc.defaultView.frameElement.clientWidth;
          h = ownerDoc.defaultView.frameElement.clientHeight;
        } else {
          w = someElement.offsetWidth;
          h = someElement.offsetHeight;
        }

        if(x + w > window.innerWidth + document.documentElement.scrollLeft - this.controller.offsetWidth - 1) {
          y += h;
        } else {
          x += w + 2;
          //y += h - ui.controller.offsetHeight - 1; // ui.controller.offsetheight is returned as its prior value, which results in incorrect UI positioning
          // so better to set the offset absolutely JMD 11/2/11
          y += h - 29; //prevent UI being positioned *above* the top of the element
        }

        if(isNaN(x) || isNaN(y)) {
          return;
        }

        this.controller.style.left = x + 'px';
        this.controller.style.top = y + 'px';
      }

      registerEvents() {
        let osk = keymanweb.osk;

        if(!osk) {
          return;
        }

        osk.addEventListener('show', (oskPosition) => {
            // Ensure that the ui.controller is visible if help is displayed
            this.controller.style.display = 'block';
            this.oskButton._setSelected(true);

            return oskPosition;
          });

        osk.addEventListener('hide', (byUser) => {
          if(byUser['HiddenByUser']) {
            this.oskButton._setSelected(false);
          }
        });
      };

      /**
       * Toggle the on screen keyboard display - KMW button control event
       **/
      readonly switchOsk = () => {
        // Check that user control of OSK is allowed
        if((keymanweb.getActiveKeyboard() == '') || keymanweb.isCJK()) {
          return;
        }

        if(keymanweb.osk) {
          const newState = !keymanweb.osk.isEnabled();
          keymanweb.osk.show(newState);

          // Also, indicate that the OSK is intentionally hidden.
          this.oskButton._setSelected(newState);
        }
      }

      /**
       * Toggle a single keyboard on or off - KMW button control event
       **/
      readonly switchSingleKbd = () => {
        const _v = keymanweb.getActiveKeyboard() == '';
        let nLastKbd=0, kbdName='', lgCode='';

        if(_v) {
          if(this.keyboards.length == 0) {
            return;
          }

          if(this.lastActiveKeyboard < this.keyboards.length && this.lastActiveKeyboard >= 0) {
            nLastKbd = this.lastActiveKeyboard;
          }

          kbdName = this.keyboards[nLastKbd]._InternalName;
          lgCode = this.keyboards[nLastKbd]._LanguageCode;
          keymanweb.setActiveKeyboard(kbdName,lgCode);
          this.lastActiveKeyboard = nLastKbd;
        } else {
          keymanweb.setActiveKeyboard('');
        }

        if(this.kbdButton) {
          this.kbdButton._setSelected(_v);
        }
      }

      /**
       * Switch to the next keyboard in the list - KMW button control event
       **/
      readonly switchNextKbd = () => {
        let _v = (keymanweb.getActiveKeyboard() == '');
        let kbdName='', lgCode='';

        if(_v) {
          if(this.keyboards.length == 0) {
            return;
          }

          kbdName = this.keyboards[0]._InternalName;
          lgCode = this.keyboards[0]._LanguageCode;
          keymanweb.setActiveKeyboard(kbdName,lgCode);
          this.lastActiveKeyboard = 0;
        } else {
          if(this.lastActiveKeyboard == this.keyboards.length-1) {
            keymanweb.setActiveKeyboard('');
            _v = false;
          } else {
            kbdName = this.keyboards[++this.lastActiveKeyboard]._InternalName;
            lgCode = this.keyboards[this.lastActiveKeyboard]._LanguageCode;
            keymanweb.setActiveKeyboard(kbdName,lgCode);
            _v = true;
          }
        }

        if(this.kbdButton) {
          this.kbdButton._setSelected(_v);
        }
      }

      /**
       * Create a button object for KeymanWeb UI buttons
       *
       * @constructor
       * @param       {string}    _src         Image source
       * @param       {string}    _caption     Alt text
       * @param       {boolean}   _selected
       * @return      {Object}
       *
       * @suppress {suspiciousCode}  // Closure isn't smart enough to realize that _onmouseover
       *                             // and the like are defined on individual instances later.
       *                             // It thinks they're always null.
       **/
      button(_src: string, _caption: string, _selected: boolean) {
        /**
         * Only ui.controllerHovered is referenced here:  it'd be easy enough to toggle it via closure
         * and extract this inner class into its own definition outside of `class ToggleUI`.
         *
         * This would also give the main class type inference for its related fields and their
         * uses.
         */
        const ui = this;

        class Button {
          // Button event-state management fields
          _onclick: () => void = null;
          _onmouseover: () => void = null;
          _onmouseout: () => void = null;
          _down = false;
          _over = false;
          _selected: boolean;

          /**
           * The top-level element of the button.
           */
          _elem: HTMLDivElement & Owned<Button>;

          getElem() {
            return this._elem;
          };

          private __updatestyle() {
            var ss=this._elem.style;
            if(this._over) {
              ss.margin = '0px';
              if(this._selected) {
                ss.border = 'solid 1px #ad4a28';
                ss.background = '#dfb4b4';
              }
              else {
                ss.border = 'solid 1px #dfb4b4';
                ss.background = '#f3e5de';
              }
            } else if(this._selected) {
              ss.background = '#f3e5de';
              ss.margin = '0px';
              ss.border = 'solid 1px #ad4a28';
            } else {
              ss.background = 'none';
              ss.margin = '1px';
              ss.border = 'none';
            }
          };

          private __mouseover = () => {
            ui.controllerHovered = true;
            this._over = true;
            if(this._onmouseover != null) {
              this._onmouseover();
            }
            this.__updatestyle();
          };

          private __mouseout = () => {
            ui.controllerHovered = false;
            this._over = false;
            if(this._onmouseout != null) {
              this._onmouseout();
            }
            this.__updatestyle();
          };

          private __click = () => {
            keymanweb.activatingUI(false); // Clear activating UI flag once click is acknowledged
            if(this._onclick != null) {
                return this._onclick();
            }
            return false;
          };

          private __mousedown = () => {
            keymanweb.activatingUI(true);  // Set activating UI flag (to manage focus/blur) on any UI mouse down event
            this._down = true;
            this.__updatestyle();
            return false;
          };

          private __mouseup = () => {
            this._down = false;
            this.__updatestyle();
          };


          _setSelected(_value: boolean) {
            keymanweb.activatingUI(false); // Always clear activating UI flag after selecting UI
            this._selected = _value;
            this.__updatestyle();
          };

          _getSelected() {
            return this._selected;
          };

          _getOver() {
            return this._over;
          };

          _getDown() {
            return this._down;
          };

          constructor(/* TODO: put wrapping method's params HERE instead upon class-extraction */) {
            this._selected = _selected;

            let imgPath=util.getOption('resources') + 'ui/toggle/';
            let _elemImg = util.createElement('img') as HTMLImageElement & Owned<Button>;
            this._elem = util.createElement('div');
            this._elem['_owningObject'] = this;
            _elemImg.style.display = 'block';
            _elemImg.src = imgPath + _src;
            _elemImg.id = 'KMW_Controller_Img';
            this._elem.style.margin = '0px'; //display = 'inline';
            this._elem.style.width = '24px';
            this._elem.style.height = '24px';
            this._elem.style.zIndex = '10002';
            this._elem.style.lineHeight = '100%';
            this._elem.style.cssFloat = 'left';

            _elemImg.title = _caption;
            _elemImg.alt = _caption;
            this._elem.appendChild(_elemImg);
            this._elem.onmouseover = this.__mouseover;
            this._elem.onmouseout = this.__mouseout;
            this._elem.onmousedown = this.__mousedown;
            this._elem.onmouseup = this.__mouseup;
            _elemImg['_owningObject'] = this;
            _elemImg.onclick = this.__click;

            this.__updatestyle();
          }
        }

        return new Button();
      };

      /**
       * Function     Initialize
       * Scope        Private
       * Description  Initialize Toggle User Interface
       **/
      initialize() {
        //Never initialize before KMW!
        if(!keymanweb.initialized || util.isTouchDevice()) {
          return;
        }

        if(!this.initialized) { // I2403 - Allow toggle design to be loaded twice
          this.controller = util.createElement('div');
        } else {
          this.controller.innerHTML = '';  // I2403 - Allow toggle design to be loaded twice
        }

        const imgPath = util.getOption('resources')+'ui/toggle/';
        this.controller.style.background = 'url('+imgPath+'kmwcontroller2x.gif)';
        this.controller.style.padding = '1px 2px';

        // Create keyboard list and OSK control buttones, and set initial styles
        const v1=util.loadCookie<KeyboardCookie>('KeymanWeb_Keyboard');
        let kbdEnabledOnLoad=false;
        if(typeof(v1.current) != 'undefined') {
          kbdEnabledOnLoad = (v1.current.indexOf('---') < 0);
        }

        this.kbdButton = this.button('kmw_logo_16.gif', 'Use Web Keyboard', kbdEnabledOnLoad);
        this.controller.appendChild(this.kbdButton.getElem());

        const v2 = util.loadCookie<Required<FloatingOSKViewCookie>>('KeymanWeb_OnScreenKeyboard');
        let oskEnabledOnLoad=true;
        if(typeof(v2.visible) != 'undefined') {
          oskEnabledOnLoad=(v2.visible == 1);
        }

        // Add keyboard icon
        this.oskButton = this.button('kmw_osk_16.gif','Show On Screen Keyboard',oskEnabledOnLoad);
        this.oskButton._onclick = this.switchOsk;
        this.controller.appendChild(this.oskButton.getElem());

        // Hide controller unless already initialized
        if(!this.initialized) {
          this.controller.style.display = 'none';
        }
        this.controller.style.zIndex = '10001';
        this.controller.style.position = 'absolute';

        // The following three lines prevent the UI from being positioned incorrectly when the page is resized,
        // but don't fix the problem completely, as the kbd icon still moves.  probably need to insert a DIV
        // between the button and the container, and make that DIV fixed height and overflow:hidden
        //ui.controller.style.maxHeight = '26px';
        //ui.oskButton.getElem().style.position = 'relative';
        //ui.oskButton.getElem().style.overflow = 'hidden';

        if(!this.initialized) { // I2403 - Allow toggle design to be loaded more than once if necessary
          document.body.appendChild(this.controller);
        }

        // Set initialized true
        this.initialized = true;  // I2403 - Allow toggle design to be loaded more than once if needed

        // Then update the keyboard list if keyboards already loaded (i.e. in page script)
        this.updateKeyboardList();
        this.registerEvents();

        // Future improvement:  have the UI modules track their own stylesheets as part of their
        // element hierarchy.  engine/dom-utils has a spun-off StylesheetManager class that's
        // perfect for this.
        util.addStyleSheet(this.stylingCSS);
      }

      shutdown() {
        var root = this.controller;

        if(root) {
          root.parentNode.removeChild(root);
        }
      }

      /**
       * Function     updateKeyboardList
       * Scope        Private
       * Description  Rebuild the UI and keyboard list
       **/
      readonly updateKeyboardList = () => {
        if(!(keymanweb.initialized || this.initialized)) {
          return; //TODO: may want to restart the timer??
        }

        this.updateList = false;

        const _kbds=keymanweb.getKeyboards();
        const imgPath=util.getOption('resources') +'ui/toggle/';

        // Check the number of installed keyboards to determine whether or not we will have a dropdown
        if(_kbds.length > 1) {
          // Multiple keyboards (button)
          const _kmw_ctrl_img=<HTMLImageElement> document.getElementById('KMW_Controller_Img');
          _kmw_ctrl_img.src = imgPath+'kmw_logo_16_down.gif';
          _kmw_ctrl_img.style.width = '100%';

          this.controller.style.background = 'url('+imgPath+'kmwcontroller2x.gif)';

          this.kbdButton.getElem().id = 'kmwico';
          this.kbdButton.getElem().style.width = '36px';
          this.kbdButton._onmouseover = () => {
            this.keyboardMenu.className="sfhover";
          };
          this.kbdButton._onmouseout = () => {
              this.keyboardMenu.className="sfunhover";
          };
          this.kbdButton._onclick = null;
          this.createMenu();
        }  else if(_kbds.length == 1) {
          // Single keyboard (button)
          const _kmw_ctrl_img=<HTMLImageElement> document.getElementById('KMW_Controller_Img')
          _kmw_ctrl_img.src = imgPath+'kmw_logo_16.gif';

          this.kbdButton.getElem().id = 'kmwico';
          this.kbdButton.getElem().style.width = '24px';

          var Lki=_kbds[0].InternalName;
          var Lklc=_kbds[0].LanguageCode;
          this.controller.style.background = 'url('+imgPath+'kmwcontroller2.gif)';
          this.keyboards.push({_InternalName: Lki, _LanguageCode: Lklc, _Index: 0});
          this.kbdButton._onclick = this.switchSingleKbd;
          this.kbdButton._onmouseover = function() { };
          this.kbdButton._onmouseout = function() { };

          // We must reconstruct the ui.keyboards array, and this done by ui.createMenu.
          this.createMenu();

          // Must remove menu if keyboards have been removed leaving only a single keyboard
          if(typeof(this.keyboardMenu) != 'undefined') {
            delete this.keyboardMenu;
          }
        }

        // Highlight the last active keyboard
        var sk=keymanweb.getSavedKeyboard().split(':');
        this.updateMenu(sk[0],sk[1]);
      }

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
      selectKbd(_kbd: number) {
        let _name,_lgCode;
        if(_kbd < 0) {
          _name = '';
          _lgCode='';
        } else {
          _name = this.keyboards[_kbd]._InternalName;
          _lgCode = this.keyboards[_kbd]._LanguageCode;
        }

        keymanweb.setActiveKeyboard(_name,_lgCode);
        keymanweb.focusLastActiveElement();
        this.kbdButton._setSelected(_name != '');
        if(_kbd >= 0) {
          this.lastActiveKeyboard = _kbd;
        }

        return false;
      };

      /**
       * Function     updateMenu
       * Scope        Private
       * @param       {string}    kbdName
       * @param       {?string=}  lgCode
       * Description  Updates the menu selection when a change is required
       **/
      updateMenu(kbdName: string, lgCode: string) {
        let _k=document.getElementById('KMWSel_$');

        for(let i=0; i < this.keyboards.length; i++) {
          if(this.keyboards[i]._InternalName == kbdName && this.keyboards[i]._LanguageCode == lgCode) {
            _k=document.getElementById('KMWSel_' + this.keyboards[i]._InternalName+'$' + this.keyboards[i]._Index);
          }
        }

        if(_k) {
          if(this.selectedMenuItem != null) {
            this.selectedMenuItem.className='';
          }
          _k.className='selected';
          // The keyboard's entry in the keyboard-list drop-down is an <a> element.
          this.selectedMenuItem=_k as HTMLAnchorElement;
        }

        // Occurs for desktop form-factors when no keyboard (aka the sys default) is active.
        if(!this.oskButton) {
          return;
        }

        // Hide the OSK button for CJK keyboards (or non-mapped)
        if(lgCode=='cmn' || lgCode=='jpn' || lgCode=='kor') {
          this.oskButton.getElem().style.display='none';
          //osk['show'](true);
        } else if(kbdName == '') {
          this.oskButton.getElem().style.display='none';
        } else {
          this.oskButton.getElem().style.display='block';
          //osk['show'](osk['isEnabled']());
        }
      }

      get stylingCSS() {
        return `
#KeymanWeb_KbdList {
  display: block;
  position: absolute;
  width: auto;
  line-height: 100%;
  margin: 0;
  clear: both;
  float: none;
  top: auto;
  border: solid 2px #ad4a28;
  -moz-border-radius: 4px;
  -webkit-border-radius: 4px;
  border-radius: 4px;
  box-shadow: 4px 4px 2px rgba(136,136,136,.5);
  -webkit-box-shadow: 4px 4px 2px rgba(136,136,136,.5);
  -moz-box-shadow: 4px 4px 2px rgba(136,136,136,.5);
  list-style: none;
  padding: 0;
  background: white;
  max-height: 300px;
  overflow-y: scroll;
  overflow-x: hidden;
  white-space: nowrap;
  z-index: 10001; /* above the osk */
}

.sfunhover#KeymanWeb_KbdList {
  display: none; left: -999px;
}

.sfhover#KeymanWeb_KbdList {
  display: block;
  left: auto;
}

#KeymanWeb_KbdList li {
  float: none;
  width: auto;
  padding: 0;
  margin: 0;
  text-align: left;
}

#KeymanWeb_KbdList li a {
  display: block;
  padding: 2px 4px;
  color: #404040;
  font-family: Tahoma,Verdana,Arial,sans-serif;
  font-size: 8pt;
  text-decoration: none;
}

#KeymanWeb_KbdList li a.selected {
  font-weight: bold;
  color: black;
}

#KeymanWeb_KbdList li a:hover {
  color: white;
  background-color: #ad4a28;
  text-decoration: underline;
}
`;
      }

      /**
       * Function     createMenu
       * Scope        Private
       * Description  Create the drop down menu and populate with loaded KeymanWeb keyboards
       **/
      createMenu() {
        if(typeof(this.keyboardMenu) == 'undefined') { // I2403 - Allow toggle design to be loaded twice
          this.keyboardMenu = util.createElement('ul');
          this.keyboardMenu.id='KeymanWeb_KbdList';
          this.keyboardMenu.className='sfunhover';
        } else {
          this.keyboardMenu.innerHTML = '';  // I2403 - Allow toggle design to be loaded twice
        }

        var _li = util.createElement('li');
        var _a = util.createElement('a');
        _a.innerHTML='(System keyboard)';
        _a.href="#";
        _a.onclick = () => {
          return this.selectKbd(-1);
        };
        _a.id='KMWSel_$';
        _a.className='selected';
        _li.appendChild(_a);

        this.selectedMenuItem=_a;
        this.keyboardMenu.appendChild(_li);

        const _kbds=keymanweb.getKeyboards(), _added: Record<string, number> = {};
        this.keyboards = [];
        for(var _kbd = 0; _kbd < _kbds.length; _kbd++) {
          var _li1=util.createElement('li');
          var _a1=util.createElement('a');
          _a1.innerHTML=_kbds[_kbd].LanguageName + ' - ' + _kbds[_kbd].Name;
          if(!_added[_kbds[_kbd].InternalName]) _added[_kbds[_kbd].InternalName]=0;
          _added[_kbds[_kbd].InternalName]++;

          var _n=_added[_kbds[_kbd].InternalName];
          this.keyboards.push({_InternalName: _kbds[_kbd].InternalName, _LanguageCode:_kbds[_kbd].LanguageCode, _Index: _n});

          _a1.href="#";
          _a1.onclick = ((x) => { return () => this.selectKbd(x); })(this.keyboards.length-1);
          _a1.id='KMWSel_'+_kbds[_kbd].InternalName+'$'+_n;

          _li1.appendChild(_a1);
          this.keyboardMenu.appendChild(_li1);
        }

        //if(!ui.initialized) // I2403 - Allow toggle design to be loaded twice
        if(this.keyboardMenu.parentNode != this.kbdButton.getElem()) {
          this.kbdButton.getElem().appendChild(this.keyboardMenu);
        }
      };

    }


    // Actually assign our UI module to the active Keyman engine.
    const ui = keymanweb.ui = new ToggleUI();

    // CTRL-K_SLASH:  toggles to and from default keyboard
    keymanweb.addHotKey(191, 0x20, ui.switchSingleKbd);

    // SHIFT-CTRL-K_SLASH:  cycles among available keyboards in sequence
    keymanweb.addHotKey(191, 0x30, ui.switchNextKbd);

    // ALT-K_SLASH:  Hides the OSK
    keymanweb.addHotKey(191, 0x40, ui.switchOsk);

    // // Initialize after KMW is fully initialized
    // keymanweb['addEventListener']('loaduserinterface', ui.initialize);

    keymanweb.addEventListener('controlfocused',function(params){
      ui.doFocus(params.target, true, params.activeControl);
    });

    keymanweb.addEventListener('controlblurred',function(params){
      ui.doFocus(params.target, false, null);
    });

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
      ui.updateTimer = window.setTimeout(ui.updateKeyboardList, 200);
    });


    /**
     * Keyboard change event handler
     *
     * Update menu selection and control OSK display appropriately
     */
    keymanweb.addEventListener('keyboardchange', function(p) {
      ui.updateMenu(p.internalName, p.languageCode);
    });

    // but also execute here, for asynchronous UI script loading (occurring after KMW initialization)
    ui.initialize();

  } catch(ex){}
}