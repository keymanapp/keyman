// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="../kmwexthtml.ts" />
// References the base KMW object.
/// <reference path="../kmwbase.ts" />
// References DOM event handling interfaces and classes.
/// <reference path="domEventHandlers.ts" />
// References DOM-specific output handling.
/// <reference path="domDefaultOutput.ts" />
// References other DOM-specific web-core overrides.
/// <reference path="domOverrides.ts" />
// Defines per-element-type OutputTarget element wrapping.
/// <reference path="targets/wrapElement.ts" />
// Defines cookie-based variable store serialization
/// <reference path="variableStoreCookieSerializer.ts" />

namespace com.keyman.dom {


  /**
   * This class serves as the intermediary between KeymanWeb and any given web page's elements.
   */
  export class DOMManager {
    private keyman: KeymanBase;

    /**
     * Implements the AliasElementHandlers interface for touch interaction.
     */
    touchHandlers?: DOMTouchHandlers;

    /**
     * Implements stubs for the AliasElementHandlers interface for non-touch interaction.
     */
    nonTouchHandlers: DOMEventHandlers;

    // Used for special touch-based page interactions re: element activation on touch devices.
    deactivateOnScroll: boolean = false;
    deactivateOnRelease: boolean = false;
    touchY: number; // For scroll-related aspects on iOS.

    touchStartActivationHandler: (e: TouchEvent) => boolean;
    touchMoveActivationHandler:  (e: TouchEvent) => boolean;
    touchEndActivationHandler:   (e: TouchEvent) => boolean;

    constructor(keyman: KeymanBase) {
      this.keyman = keyman;

      if(keyman.util.device.touchable) {
        this.touchHandlers = new DOMTouchHandlers(keyman);
      }

      this.nonTouchHandlers = new DOMEventHandlers(keyman);
    }

    shutdown() {
      // Catch and notify of any shutdown errors, but don't let errors fail unit tests.
      try {
        if(this.enablementObserver) {
          this.enablementObserver.disconnect();
        }
        if(this.attachmentObserver) {
          this.attachmentObserver.disconnect();
        }

        // On shutdown, we remove our general focus-suppression handlers as well.
        this.keyman.util.detachDOMEvent(document.body, 'focus', DOMManager.suppressFocusCheck, true);
        this.keyman.util.detachDOMEvent(document.body, 'blur', DOMManager.suppressFocusCheck, true);

        // Also, the base-page touch handlers for activation management.
        if(this.touchStartActivationHandler) {
          this.keyman.util.detachDOMEvent(document.body, 'touchstart', this.touchStartActivationHandler, false);
          this.keyman.util.detachDOMEvent(document.body, 'touchmove',  this.touchMoveActivationHandler,  false);
          this.keyman.util.detachDOMEvent(document.body, 'touchend',   this.touchEndActivationHandler,   false);
        }
      } catch (e) {
        console.error("Error occurred during shutdown");
        console.error(e);
      }
    }

    /* ------ End independent, per-control keyboard setting behavior definitions. ------ */

    get activeElement(): HTMLElement {
      return DOMEventHandlers.states._activeElement;
    }

    set activeElement(Pelem: HTMLElement) {
      DOMEventHandlers.states._activeElement = Pelem;

      let maintainingFocus = focusAssistant.maintainingFocus;

      // Hide the OSK when the control is blurred, unless the UI is being temporarily selected
      const osk = this.keyman.osk;
      // const device = this.keyman.util.device;

      if(osk) {
        const target = Pelem?._kmwAttachment?.interface || null;
        if(osk && osk.activationModel instanceof TwoStateActivator && (target || !maintainingFocus)) {
          // Do not unset the field if the UI is activated.
          osk.activationCondition = target;
        }
      }
    }

    /**
     *  Set the active input element directly optionally setting focus
     *
     *  @param  {Object|string} e         element id or element
     *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123)
     */
    setActiveElement(e: string|HTMLElement, setFocus?: boolean) {
      if(typeof e == "string") { // Can't instanceof string, and String is a different type.
        e = document.getElementById(e);
      }

      if(this.keyman.isEmbedded) {
        // If we're in embedded mode, auto-attach to the element specified by the page.
        if(!this.isAttached(e)) {
          this.attachToControl(e);
        }
        // Non-attached elements cannot be set as active.
      } else if(!this.isAttached(e)) {
        console.warn("Cannot set an element KMW is not attached to as the active element.");
        return;
      }

      // If we're changing controls, don't forget to properly manage the keyboard settings!
      // It's only an issue on 'native' (non-embedded) code paths.
      if(!this.keyman.isEmbedded) {
        this.keyman.touchAliasing._BlurKeyboardSettings(this.keyman.domManager.lastActiveElement);
      }

      // No need to reset context if we stay within the same element.
      if(this.activeElement != e) {
        this.keyman['resetContext'](e as HTMLElement);
      }

      this.activeElement = this.lastActiveElement = e;
      if(!this.keyman.isEmbedded) {
        this.keyman.touchAliasing._FocusKeyboardSettings(e, false);
      }

      // Allow external focusing KMEW-123
      if(arguments.length > 1 && setFocus) {
        this.focusLastActiveElement();
      }

      // Let the keyboard do its initial group processing
      //console.log('processNewContextEvent [not] called from setActiveElement');
      com.keyman.singleton.core.processNewContextEvent(dom.Utils.getOutputTarget(e));
    }

    /** Sets the active input element only if it is presently null.
     *
     * @param  {Element}
     */
    initActiveElement(Lelem: HTMLElement) {
      if(this.activeElement == null) {
        this.activeElement = Lelem;
      }
    }

    /* ----------------------- Editable IFrame methods ------------------- */

    /**
     * Function     _IsEditableIframe
     * Scope        Private
     * @param       {Object}           Pelem    Iframe element
     * @param       {boolean|number}   PtestOn  1 to test if 'designMode' is 'ON'
     * @return      {boolean}
     * Description  Test if element is a Mozilla editable IFrame
     */
    _IsEditableIframe(Pelem: HTMLIFrameElement, PtestOn?: number) {
      var Ldv, Lvalid = Pelem  &&  (Ldv=(<any>Pelem).defaultView)  &&  Ldv.frameElement;  // Probable bug!
      return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Ldv.document.designMode.toLowerCase()=='on'));
    }

    /* ----------------------- Initialization methods ------------------ */

    /**
     * Get the user-specified (or default) font for the first mapped input or textarea element
     * before applying any keymanweb styles or classes
     *
     *  @return   {string}
     */
    getBaseFont() {
      var util = this.keyman.util;
      var ipInput = document.getElementsByTagName<'input'>('input'),
          ipTextArea=document.getElementsByTagName<'textarea'>('textarea'),
          n=0,fs,fsDefault='Arial,sans-serif';

      // Find the first input element (if it exists)
      if(ipInput.length == 0 && ipTextArea.length == 0) {
        n=0;
      } else if(ipInput.length > 0 && ipTextArea.length == 0) {
        n=1;
      } else if(ipInput.length == 0 && ipTextArea.length > 0) {
        n=2;
      } else {
        var firstInput = ipInput[0];
        var firstTextArea = ipTextArea[0];

        if(firstInput.offsetTop < firstTextArea.offsetTop) {
          n=1;
        } else if(firstInput.offsetTop > firstTextArea.offsetTop) {
          n=2;
        } else if(firstInput.offsetLeft < firstTextArea.offsetLeft) {
          n=1;
        } else if(firstInput.offsetLeft > firstTextArea.offsetLeft) {
          n=2;
        }
      }

      // Grab that font!
      switch(n) {
        case 0:
          fs=fsDefault;
        case 1:
          fs=util.getStyleValue(ipInput[0],'font-family');
        case 2:
          fs=util.getStyleValue(ipTextArea[0],'font-family');
      }
      if(typeof(fs) == 'undefined' || fs == 'monospace') {
        fs=fsDefault;
      }

      return fs;
    }

    /**
     * Function     Initialization
     * Scope        Public
     * @param       {com.keyman.OptionType}  arg     object of user-defined properties
     * Description  KMW window initialization
     */
    init: (arg: com.keyman.OptionType) => Promise<any> = function(this: DOMManager, arg): Promise<any> {
      var p,opt,dTrailer,ds;
      var util = this.keyman.util;
      var device = util.device;

      // Set callbacks for proper feedback from web-core.
      this.keyman.core.keyboardProcessor.beepHandler = this.doBeep.bind(this);
      this.keyman.core.keyboardProcessor.warningLogger = console.warn.bind(console);
      this.keyman.core.keyboardProcessor.errorLogger = console.error.bind(console);

      // Set default device options
      this.keyman.setDefaultDeviceOptions(opt);

      // Only do remainder of initialization once!
      if(this.keyman.initialized) {
        return Promise.resolve();
      }

      this.keyman.linkStylesheetResources();

      const keyman: KeymanBase = this.keyman;
      const domManager = this;

      // Do not initialize until the document has been fully loaded
      if(document.readyState !== 'complete')
      {
        return new Promise<void>(function(resolve) {
          window.setTimeout(function(){
            domManager.init(arg).then(function() {
              resolve();
            });
          }, 50);
        });
      }

      keyman.modelManager.init();
      this.keyman._MasterDocument = window.document;

      /*
       * Initialization of touch devices and browser interfaces must be done
       * after all resources are loaded, during final stage of initialization
       */

      // Set exposed initialization flag member for UI (and other) code to use
      this.keyman.setInitialized(1);

      // Finish keymanweb and initialize the OSK once all necessary resources are available
      // OSK type selection is already modularized... but the ordering related to the parts
      // afterward, which are not yet modularized, may be important.
      if(device.touchable) {
        this.keyman.osk = new com.keyman.osk.AnchoredOSKView(device.coreSpec);
      } else {
        this.keyman.osk = new com.keyman.osk.FloatingOSKView(device.coreSpec);
      }
      const osk = this.keyman.osk;

      // Create and save the remote keyboard loading delay indicator
      util.prepareWait();

      // Trigger registration of deferred keyboard stubs and keyboards
      this.keyman.keyboardManager.endDeferment();

      // Initialize the desktop UI
      this.initializeUI();

      // Determine the default font for mapped elements
      this.keyman.appliedFont=this.keyman.baseFont=this.getBaseFont();

      // Add orientationchange event handler to manage orientation changes on mobile devices
      // Initialize touch-screen device interface  I3363 (Build 301)
      if(device.touchable) {
        this.keyman.handleRotationEvents();
      }
      // Initialize browser interface

      // Modular form:  pageContextAttachment.install()

      // Initialize the OSK and set default OSK styles
      // Note that this should *never* be called before the OSK has been initialized.
      // However, it possibly may be called before the OSK has been fully defined with the current keyboard, need to check.
      //osk._Load();

      //document.body.appendChild(osk._Box);

      //osk._Load(false);

      // I3363 (Build 301)
      if(device.touchable) {
        const osk = keyman.osk as osk.AnchoredOSKView;
        // Handle OSK touchend events (prevent propagation)
        osk._Box.addEventListener('touchend',function(e){
          e.stopPropagation();
        }, false);
      }

      //document.body.appendChild(keymanweb._StyleBlock);

      // Restore and reload the currently selected keyboard, selecting a default keyboard if necessary.
      this.keyman.keyboardManager.restoreCurrentKeyboard();

      // Set exposed initialization flag to 2 to indicate deferred initialization also complete

      // Other initialization details after this point have already been modularized:
      // within app/browser KeymanEngine.init, see `setupOskListeners` call and after.

      this.keyman.setInitialized(2);
      return Promise.resolve();
    }.bind(this);

    /**
     * Initialize the desktop user interface as soon as it is ready
     */
    initializeUI() {
      if(this.keyman.ui && this.keyman.ui['initialize'] instanceof Function) {
        this.keyman.ui['initialize']();
        // Display the OSK (again) if enabled, in order to set its position correctly after
        // adding the UI to the page
        this.keyman.osk.present();
      } else if(this.keyman.isEmbedded) {
        // UI modules aren't utilized in embedded mode.  There's nothing to init, so we simply
        // return instead of waiting for a UI module that will never come.
        return;
      } else {
        window.setTimeout(this.initializeUI.bind(this),1000);
      }
    }
  }
}