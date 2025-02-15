/***
   KeymanWeb 17.0
   Copyright 2019-2023 SIL International
***/

import type { KeymanEngine, UIModule } from 'keyman/app/browser';

declare var keyman: KeymanEngine

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!keyman?.ui?.name) {
  /********************************/
  /*                              */
  /* Floating User Interface      */
  /*                              */
  /********************************/

  /**
   * Do not enclose in an anonymous function, as the compiler may create
   * global scope variables to replace true, false, null, which can collide
   * with other variables.
   * Instead, use the --output-wrapper command during optimization, which will
   * add the anonymous function to enclose all code, including those optimized
   * variables which would otherwise have global scope.
   **/

  try {

    // Declare KeymanWeb, OnScreen keyboard and Util objects
    const keymanweb = keyman;
    const util=keymanweb.util;

    // Disable UI for touch devices
    if(util.isTouchDevice()) {
      throw '';
    }

    class UIFloat implements UIModule {
      // User interface global and local variables
      readonly name = 'float';

      /**
       * The top-level element for the keyboard-selection drop menu
       */
      KeyboardSelector: HTMLSelectElement = null;

      /**
       * Container for all Float UI elements (visible when KeymanWeb is active)
       */
      outerDiv: HTMLDivElement = null;     // replaces DivKeymanWeb

      innerDiv: HTMLDivElement = null;     // replaces Lkdiv

      /**
       * The button element allowing users to toggle the OSK on or off
       */
      oskButton: HTMLDivElement = null;

      /**
       * keyboard icon within the OSK button
       */
      kbdIcon: HTMLImageElement = null;

      /**
       * Tracks ongoing user interactions with the Float UI.  This is used to help
       * control focus-related behaviors during selection so that KMW may more easily
       * maintain the user's intended focus & expected context.
       */
      selecting = false;

      updateList = true;   // control keyboard list updating
      updateTimer: number = 0;  // prevent unnecessary list refreshing
      floatRight = false;  // align left by default
      initialized = false; // initialization flag
      initTimer: number = 0;

      /**
       * Display or hide the OSK from the OSK icon link
       */
      readonly toggleOSK = () => {
        keymanweb.activatingUI(true);
        let osk = keymanweb.osk;
        if(osk && osk.show) {
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
        keymanweb.activatingUI(false);
        return false;
      }

      /**
       * Function     Initialize
       * Scope        Private
       * Description  UI Initialization
       **/
      readonly initialize = () => {
        if(this.initTimer) {
          window.clearTimeout(this.initTimer);
          this.initTimer = null;
        }

        // Must always initialize after keymanWeb itself, otherwise options are undefined
        if(!keymanweb.initialized) {
          this.initTimer = window.setTimeout(this.initialize, 50);
          return;
        }

        if(this.initialized || util.isTouchDevice()) {
          return;
        }

        const imgPath=util.getOption('resources')+"ui/float/";

        this.outerDiv = util.createElement('div');         // Container for UI (visible when KeymanWeb is active)
        this.innerDiv = util.createElement('div');         // inner div for UI
        this.kbdIcon = util.createElement('img');
        this.outerDiv.innerHTML = "<a href='http://keyman.com/web/' target='KeymanWebHelp'>"
          + "<img src='"+imgPath+"kmicon.gif' border='0' style='padding: 0px 2px 0 1px; margin:0px;' title='KeymanWeb' alt='KeymanWeb' /></a>"; /* I2081 */

        var s=this.outerDiv.style;
        s.backgroundColor='white'; s.border='solid 1px black'; s.position='absolute'; s.height='18px';
        s.font='bold 8pt sans-serif'; s.display='none'; s.textAlign='left';s.overflow='hidden';

        util.attachDOMEvent(this.outerDiv, 'mousedown', this._SelectorMouseDown);
        util.attachDOMEvent(this.outerDiv, 'mouseover', this._SelectorMouseOver);
        util.attachDOMEvent(this.outerDiv, 'mouseout',  this._SelectorMouseOut);

        // Register keymanweb events
        this.registerEvents();

        this.kbdIcon.src = imgPath+'kbdicon.gif';
        this.kbdIcon.title = 'Display visual keyboard';

        // Set initial OSK button style (off by default)
        this.oskButtonState(false);

        var Lhdiv = util.createElement('div');
        this.oskButton = Lhdiv;
        Lhdiv.onclick = this.toggleOSK;
        Lhdiv.appendChild(this.kbdIcon);
        this.innerDiv.appendChild(Lhdiv);
        this.outerDiv.appendChild(this.innerDiv);
        document.body.appendChild(this.outerDiv);

        this.KeyboardSelector =  util.createElement('select'); // ControlSelector - KeymanWeb keyboard selector

        s=this.KeyboardSelector.style;
        s.font='8pt sans-serif';
        s.backgroundColor='#f3e5de';
        s.border='solid 1px #7B9EBD';
        s.height='16px';
        s.margin='1px 2px 0px 2px';
        s.left='20px';
        s.top='0px';
        s.position='absolute';
        s.maxWidth='150px';

        util.attachDOMEvent(this.KeyboardSelector, 'change', this.SelectKeyboardChange);
        util.attachDOMEvent(this.KeyboardSelector, 'blur',   this.SelectBlur);

        this.innerDiv.appendChild(this.KeyboardSelector);  //this may need to be moved up....

        // Check required interface alignment and default keyboard
        var opt=util.getOption('ui'), dfltKeyboard='(System keyboard)';
        if(opt && typeof(opt) == 'object') {
          if(typeof(opt['position']) == 'string' && opt['position'] == 'right') {
            this.floatRight = true;
          } if(typeof(opt['default']) == 'string') {
            dfltKeyboard = opt['default'];
          }
        }

        var Lopt = util.createElement('option');
        Lopt.value = '-';
        Lopt.innerHTML = dfltKeyboard;
        this.KeyboardSelector.appendChild(Lopt);
        Lopt = null;

        // This must be set before updating the keyboard list to prevent recursion!
        this.initialized = true;

        // Update the keyboard list if required
        this.updateKeyboardList();

        //may also want to initialize style sheet here ??
      }

      readonly _UnloadUserInterface = () => {
        this.KeyboardSelector = this.innerDiv = this.outerDiv = this.kbdIcon = null;
      };

      /**
       * UI removal - resource cleanup
       */
      shutdown() {
        var root = this.outerDiv;
        if(root) {
          root.parentNode.removeChild(root);
        }

        this._UnloadUserInterface();

        if(window.removeEventListener) {
          window.removeEventListener('resize', this._Resize, false);
        }
      }

      /**
       * Update list of keyboards shown by UI
       */
      readonly updateKeyboardList = () => {
        // Do nothing unless list requires updating
        if(this.updateList) {
          if(!this.initialized) {
            this.initialize();
          }

          // Remove current list (except for default element)
          const opts=this.KeyboardSelector.getElementsByTagName('OPTION');
          for(let i=opts.length; i>1; i--) {
            this.KeyboardSelector.removeChild(opts[i-1]);
          }

          // Loop over installed keyboards and add to selection list
          var Lkbds=keymanweb.getKeyboards();

          for(let Ln=0; Ln<Lkbds.length; Ln++) {
            let Lopt = util.createElement('option');
            Lopt.value = Lkbds[Ln].InternalName+':'+Lkbds[Ln].LanguageCode;
            Lopt.innerHTML = Lkbds[Ln].Name.replace(/\s?keyboard/i,'');
            if(Lkbds[Ln].LanguageName) {
              var lg=Lkbds[Ln].LanguageName;
              // Only show the main language name if variants indicated (this is tentative)
              // e.g. Chinese rather than Chinese, Mandarin, which is in the keyboard name
              lg = lg.split(',')[0];
              if(Lkbds[Ln].Name.search(lg) == -1) {
                Lopt.innerHTML = lg+' ('+Lopt.innerHTML+')';  // I1300 - Language support
              }
            }

            this.KeyboardSelector.appendChild(Lopt);
            Lopt = null;
          }
        }
        this.updateList = false;

        // Set the menu selector to the currently saved keyboard
        const sk = keymanweb.getSavedKeyboard().split(':');
        if(sk.length < 2) {
          sk[1] = '';
        }
        this.updateMenu(sk[0],sk[1]);

        // Redisplay the UI to correct width for any new language entries
        if(keymanweb.getLastActiveElement()) {
          this.HideInterface();
          this.ShowInterface();
        }
      }

      /**
       * Function     updateMenu
       * Scope        Private
       * @param       {string}     kbd
       * @param       {string}     lg
       * Description  Update the UI menu selection
       *              Separate arguments passed to allow better control of selection when a keyboard
       *              is listed more than once for different language codes
       */
      readonly updateMenu = (kbd: string, lg: string) => {
        var i=0;

        // This can be called during startup before fully initialized - ignore if so
        if(!this.initialized) {
          return;
        }

        var match = kbd;
        if(lg != '') {
          match += ':' + lg;
        }

        if(kbd == '') {
          this.KeyboardSelector.selectedIndex=0;
        } else {
          const opt=this.KeyboardSelector.getElementsByTagName('option');
          for(i=0; i<opt.length; i++) {
            let t=opt[i].value;
            if(lg == '') {
              t = t.split(':')[0];
            }

            if(t == match) {
              this.KeyboardSelector.selectedIndex=i;
              break;
            }
          }
        }
      }

      /**
       * Function     oskButtonState
       * Scope        Private
       * @param       {boolean}     oskEnabled
       * Description  Update kbd icon border style to indicate whether OSK is enabled for display or not
       **/
      readonly oskButtonState = (oskEnabled: boolean) => {
        var s = this.kbdIcon.style;
        s.width='24px';
        s.height='13px';
        s.top='1px';
        s.verticalAlign='bottom';
        s.padding='2px 2px 1px 2px';
        s.position='absolute';
        s.border=oskEnabled ? 'inset 1px #808080' : 'none';
        s.background=oskEnabled ? '#f7e7de' : 'white';
        s.display = 'block';
        if(this.initialized) {
          this.oskButton.style.display = 'block';
        }
      }

      /**
       * Register all keymanweb and OSK events only after keymanweb is fully initialized
       *
       **/
      registerEvents() {
        /**
         * Keyboard registration event handler
         *
         * Set a timer to update the UI keyboard list on timeout after each keyboard is registered,
         * thus updating only once when only if multiple keyboards are registered together
         */
        keymanweb.addEventListener('keyboardregistered', (p) => {
          this.updateList = true;
          if(this.updateTimer) {
            clearTimeout(this.updateTimer);
          }
          this.updateTimer = window.setTimeout(this.updateKeyboardList, 200);
        });

        /**
         * Keyboard load event handler
         *
         * Set the menu selector to the currently saved keyboard when a keyboard is loaded
         *
         * Note: Cannot simply set it to the loaded keyboard,
         *       as more than one language may be supported by that keyboard.
         */
        keymanweb.addEventListener('keyboardloaded', (p) => {
          const sk = keymanweb.getSavedKeyboard().split(':');
          if(sk.length > 1) {
            this.updateMenu(sk[0],sk[1]);
          }
        });

        /**
         * Keyboard change event handler
         *
         * Update menu selection and control OSK display appropriately
         */
        keymanweb.addEventListener('keyboardchange', (p) => {
          // Update the keyboard selector whenever a keyboard is loaded
          this.updateMenu(p.internalName, p.languageCode);

          // (Conditionally) display the OSK button, and set the style
          this.addButtonOSK();
        });

        let osk = keymanweb.osk;
        if(!osk) {
          return;
        }

        /**
         * Show OSK event handler: show or hide the OSK button (never display if a CJK keyboard)
         */
        osk.addEventListener('show', (oskPosition) => {
          this.addButtonOSK();
          return oskPosition;
        });

        /**
         * Hide OSK event handler
         */

        osk.addEventListener('hide', (hiddenByUser) => {
          if(this.initialized) {
            this.oskButtonState(false);
          }
        });
      }

      /**
       * Function     _SelectorMouseDown
       * Scope        Private
       * @param       {Object}     e      event
       * Description  Set KMW UI activation state on mouse click
       */
      readonly _SelectorMouseDown = (e: MouseEvent) => {
        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(1);
        }
      }

      /**
       * Function     _SelectorMouseOver
       * Scope        Private
       * @param       {Object}    e       event
       * Description  Set KMW UI activation state on mouse over
       */
      readonly _SelectorMouseOver = (e: MouseEvent) => {
        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(1);
        }
      }

      /**
       * Function     _SelectorMouseOut
       * Scope        Private
       * @param       {Object}    e       event
       * Description Clear KMW UI activation state on mouse out
       */
      readonly _SelectorMouseOut = (e: MouseEvent) => {
        if(keymanweb.activatingUI) {
          keymanweb.activatingUI(0);
        }
      }

      /**
       * Function     _SelectKeyboardChange
       * Scope        Private
       * @param       {Object}    e       event
       * Description  Change active keyboard in response to user selection event
       */
      readonly SelectKeyboardChange = async (e: Event) => {
        keymanweb.activatingUI(true);

        if(this.KeyboardSelector.value != '-') {
          var i=this.KeyboardSelector.selectedIndex;
          var t=this.KeyboardSelector.options[i].value.split(':');
          await keymanweb.setActiveKeyboard(t[0],t[1]);
        } else {
          await keymanweb.setActiveKeyboard('');
        }

        //if(osk['show']) osk['show'](osk['isEnabled']()); handled by keyboard change event???
        keymanweb.focusLastActiveElement();
        keymanweb.activatingUI(false);
        this.selecting = true;
      }

      /**
       * Function     _SelectBlur
       * Scope        Private
       * @param       {Object}    e       event
       * Description  Ensure OSK is hidden when moving focus after reselecting a keyboard
       */
      readonly SelectBlur = (e: Event) => {
        if(!this.selecting) {
          keymanweb.focusLastActiveElement();
        }
        this.selecting = false;
      }

      /**
       * Function     ShowInterface
       * Scope        Private
       * @param       {number=}    Px    x-position for KMW window
       * @param       {number=}    Py    y-position for KMW window
       * Description  Display KMW window at specified position
       */
      readonly ShowInterface = (Px?: number, Py?: number) => {
        if(!this.initialized) return;

        var Ls = this.outerDiv.style;

        if(Px  &&  Py) {
          Ls.left = Px + 'px';
          Ls.top = Py + 'px';
        }
        Ls.display = 'block';

        this.kbdIcon.style.left = this.KeyboardSelector.offsetWidth + 24 + 'px';

        // (Conditionally) display the OSK button
        this.addButtonOSK();

        // Set the language selection to the currently active keyboard, if listed
        this.updateMenu(keymanweb.getActiveKeyboard(), keymanweb.getActiveLanguage());
      }

      /**
       * Function     HideInterface
       * Scope        Private
       * Description  Completely hide KMW window
       */
      readonly HideInterface = () => {
        if(!this.initialized) {
          return;
        }

        this.outerDiv.style.display = 'none';
      }

      /**
       * Function     addButtonOSK
       * Scope        Private
       * Description  Display the OSK button unless a CJK keyboard (or English)
       */
      readonly addButtonOSK = () => {
        if(this.oskButton != null) {
          if(keymanweb.isCJK() || (this.KeyboardSelector.selectedIndex==0)) {
            this.oskButton.style.display = 'none';
            this.outerDiv.style.width = this.KeyboardSelector.offsetWidth+30+'px';
          } else {
            this.oskButton.style.display = 'block';
            let osk = keymanweb.osk;
            if(osk) {
              this.oskButtonState(osk.isEnabled());
            } else {
              this.oskButtonState(false);
            }
            this.outerDiv.style.width = this.KeyboardSelector.offsetWidth+56+'px';
          }
        }
      }

      /**
       * Function     _Resize
       * Scope        Private
       * @param       {Object}     e      event object
       * @return      {boolean}
       * Description  Display KMW OSK at specified position (returns nothing)
       */
      readonly _Resize = (e: Event) => {
        if(this.outerDiv.style.display =='block') {
          var elem = keymanweb.getLastActiveElement();
          if(this.floatRight) {  // I1296
            this.ShowInterface(util.getAbsoluteX(elem) + elem.offsetWidth + 1, util.getAbsoluteY(elem) + 1);
          } else {
            this.ShowInterface(util.getAbsoluteX(elem) + 1, util.getAbsoluteY(elem) + elem.offsetHeight + 1);
          }
        }
        return true;
      }
    }

    const ui=keymanweb.ui = new UIFloat();

    //TODO:  had to expose properties of params - what does that do? (focus event doesn't normally include these properties?)
    keymanweb.addEventListener('controlfocused', (params) => {
      // ... this check shouldn't need to check _kmwAttachment directly.
      if(params.activeControl == null || params.activeControl['_kmwAttachment']) {
        /*if(keymanweb.domManager._IsEditableIframe(Ltarg))
          Ltarg = Ltarg.defaultView.frameElement;*/
        if(ui.floatRight) {  // I1296
          ui.ShowInterface(util.getAbsoluteX(params.target) + params.target.offsetWidth + 1, util.getAbsoluteY(params.target) + 1);
        } else {
          ui.ShowInterface(util.getAbsoluteX(params.target), util.getAbsoluteY(params.target)
            - parseInt(util.getStyleValue(ui.outerDiv,'height'),10) - 2);
        }
      }
      return true;
    });

    keymanweb.addEventListener('controlblurred', (params) => {
      if(!params.event) {
        return true;   // I2404 - Manage IE events in IFRAMEs
      }

      if(!params.isActivating) {
        ui.HideInterface();
      }

      return true;
    });

    if(window.addEventListener) {
      window.addEventListener('resize', ui._Resize, false);
    }

    // but also call initialization when script loaded, which is after KMW initialization for asynchronous script loading
    ui.initialize();

    // is/was never actually raised.  Note that the `shutdown` method likely fulfills a similar role.
    // keymanweb.addEventListener('unloaduserinterface', ui._UnloadUserInterface);

  } catch(err){}

  // Do not wrap in an anonymous function - let the closure compiler do that, but
  // use try...catch to avoid script errors and only execute if a desktop browser
}