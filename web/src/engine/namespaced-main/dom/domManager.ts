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
  // Utility object used to handle beep (keyboard error response) operations.
  class BeepData {
    e: HTMLElement;
    c: string;

    constructor(e: HTMLElement) {
      this.e = e;
      this.c = e.style.backgroundColor;
    }

    reset(): void {
      this.e.style.backgroundColor = this.c;
    }
  }

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

    /**
     * Tracks the attachment MutationObserver.
     */
    attachmentObserver: MutationObserver;

    /**
     * Tracks the enablement MutationObserver.
     */
    enablementObserver: MutationObserver;

    /**
     * Tracks changes in inputmode state.
     */
    inputModeObserver: MutationObserver;

    /**
     * Tracks a list of event-listening elements.
     *
     * In touch mode, this should contain touch-aliasing DIVs, but will contain other elements in non-touch mode.
     */
    inputList: HTMLElement[] = [];            // List of simulated input divisions for touch-devices   I3363 (Build 301)

    /**
     * Tracks a visually-sorted list of elements that are KMW-enabled.
     */
    sortedInputs: HTMLElement[] = [];   // List of all INPUT and TEXTAREA elements ordered top to bottom, left to right

    _BeepObjects: BeepData[] = [];  // BeepObjects - maintains a list of active 'beep' visual feedback elements
    _BeepTimeout: number = 0;       // BeepTimeout - a flag indicating if there is an active 'beep'.
                                    // Set to 1 if there is an active 'beep', otherwise leave as '0'.

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
        if(this.inputModeObserver) {
          this.inputModeObserver.disconnect();
        }

        for(let input of this.inputList) {
          this.disableInputElement(input);
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

    /**
     * Function     beep          KB      (DOM-side implementation)
     * Scope        Public
     * @param       {Object}      Pelem     element to flash
     * Description  Flash body as substitute for audible beep; notify embedded device to vibrate
     */
    doBeep(outputTarget: targets.OutputTarget) {
      // Handles embedded-mode beeps.
      let keyman = com.keyman.singleton;
      if ('beepKeyboard' in keyman) {
        keyman['beepKeyboard']();
        return;
      }

      if(!(outputTarget instanceof targets.OutputTarget)) {
        return;
      }

      // All code after this point is DOM-based, triggered by the beep.
      var Pelem: HTMLElement = outputTarget.getElement();
      if(outputTarget instanceof dom.targets.DesignIFrame) {
        Pelem = outputTarget.docRoot; // I1446 - beep sometimes fails to flash when using OSK and rich control
      }

      if(!Pelem) {
        return; // There's no way to signal a 'beep' to null, so just cut everything short.
      }

      if(!Pelem.style || typeof(Pelem.style.backgroundColor)=='undefined') {
        return;
      }

      for(var Lbo=0; Lbo<this._BeepObjects.length; Lbo++) { // I1446 - beep sometimes fails to return background color to normal
                                                                  // I1511 - array prototype extended
        if(this._BeepObjects[Lbo].e == Pelem) {
          return;
        }
      }

      this._BeepObjects = com.keyman.singleton._push(this._BeepObjects, new BeepData(Pelem));
      // TODO:  This is probably a bad color choice if "dark mode" is enabled.  A proper implementation
      //        would probably require some 'fun' CSS work, though.
      Pelem.style.backgroundColor = '#000000';
      if(this._BeepTimeout == 0) {
        this._BeepTimeout = 1;
        window.setTimeout(this.beepReset.bind(this), 50);
      }
    }

    /**
     * Function     beepReset
     * Scope        Public
     * Description  Reset/terminate beep or flash (not currently used: Aug 2011)
     */
    beepReset(): void {
      com.keyman.singleton.core.keyboardInterface.resetContextCache();

      var Lbo;
      this._BeepTimeout = 0;
      for(Lbo=0;Lbo<this._BeepObjects.length;Lbo++) { // I1511 - array prototype extended
        this._BeepObjects[Lbo].reset();
      }
      this._BeepObjects = [];
    }

    /**
     * Function     getHandlers
     * Scope        Private
     * @param       {Element}   Pelem  An input, textarea, or touch-alias element from the page.
     * @returns     {Object}
     */
    getHandlers(Pelem: HTMLElement): DOMEventHandlers {
      var _attachObj = Pelem.base ? Pelem.base._kmwAttachment : Pelem._kmwAttachment;

      if(_attachObj) {
        return _attachObj.touchEnabled ? this.touchHandlers : this.nonTouchHandlers;
      } else {
        // Best guess solution.
        return this.keyman.touchAliasing;
      }
    }

    /**
     * Function     enableTouchElement
     * Scope        Private
     * @param       {Element}  Pelem   An input or textarea element from the page.
     * @return      {boolean}  Returns true if it creates a simulated input element for Pelem; false if not.
     * Description  Creates a simulated input element for the specified INPUT or TEXTAREA, comprising:
     *              an outer DIV, matching the position, size and style of the base element
     *              a scrollable DIV within that outer element
     *              two SPAN elements within the scrollable DIV, to hold the text before and after the caret
     *
     *              The left border of the second SPAN is flashed on and off as a visible caret
     *
     *              Also ensures the element is registered on keymanweb's internal input list.
     */
    enableTouchElement(Pelem: HTMLElement) {
      if(this.isKMWDisabled(Pelem)) {
        this.setupNonKMWTouchElement(Pelem);
        return false;
      } else {
        // Initialize and protect input elements for touch-screen devices (but never for apps)
        // NB: now set disabled=true rather than readonly, since readonly does not always
        // prevent element from getting focus, e.g. within a LABEL element.
        // c.f. http://kreotekdev.wordpress.com/2007/11/08/disabled-vs-readonly-form-fields/
        Pelem.kmwInput = true;
      }

      // Remove any handlers for "NonKMWTouch" elements, since we're enabling it here.
      Pelem.removeEventListener('touchstart', this.nonKMWTouchHandler);

      if(!this.isAttached(Pelem)) {
        this.setupElementAttachment(Pelem);
      }

      // Set font for base element
      this.enableInputElement(Pelem);

      // Superimpose custom input fields for each input or textarea, unless readonly or disabled

      // On touch event, reposition the text caret and prepare for OSK input
      // Removed 'onfocus=' as that resulted in handling the event twice (on iOS, anyway)

      // We know this to be the correct set of handlers because we're setting up a touch element.
      var touchHandlers = this.touchHandlers;

      Pelem.addEventListener('touchmove', (event) => {
        // Prevent the base-page `touchMoveActivationHandler` from triggering if we're
        // "scrolling" within a valid input-element type.  That can be used for selection.
        event.stopPropagation();
      }, false);
      Pelem.addEventListener('touchend', touchHandlers.dragEnd, false);

      // Note that touchend event propagates and is processed by body touchend handler
      // re-setting the first touch point for a drag

      return true;
    }

    /**
     * Function     disableTouchElement
     * Scope        Private
     * @param       {Element}  Pelem   An input or textarea element from the page.
     * Description  Destroys the simulated input element for the specified INPUT or TEXTAREA and reverts
     *              back to desktop-style 'enablement' for the base control.
     */
    disableTouchElement(Pelem: HTMLElement) {
      // Do not check for the element being officially disabled - it's also used for detachment.
      if(this.isAttached(Pelem)) {
        const intendedInputMode = Pelem._kmwAttachment.inputMode;

        this.disableInputModeObserver();
        Pelem.inputMode = intendedInputMode;
        this.enableInputModeObserver();
      }

      this.setupNonKMWTouchElement(Pelem);
    }

    /**
     * Function     nonKMWTouchHandler
     * Scope        Private
     * Description  A handler for KMW-touch-disabled elements when operating on touch devices.
     */
    nonKMWTouchHandler = function(x) {
      DOMEventHandlers.states.focusing=false;
      clearTimeout(DOMEventHandlers.states.focusTimer);
      this.keyman.osk.hideNow();
    }.bind(this);

    /**
     * Function     setupNonKMWTouchElement
     * Scope        Private
     * @param       {Element}    x  A child element of document.
     * Description  Performs handling for the specified disabled input element on touch-based systems.
     */
    setupNonKMWTouchElement(x: HTMLElement) {
      this.keyman.util.attachDOMEvent(x, 'touchstart', this.nonKMWTouchHandler, false);

      // Signify that touch isn't enabled on the control.
      if(this.isAttached(x)) {
        x._kmwAttachment.touchEnabled = false;
      }
    }

    /**
     * Function     enableInputElement
     * Scope        Private
     * @param       {Element}   Pelem   An element from the document to be enabled with full KMW handling.
     * @param       {boolean=}   isAlias A flag that indicates if the element is a simulated input element for touch.
     * Description  Performs the basic enabling setup for one element and adds it to the inputList if it is an input element.
     *              Note that this method is called for both desktop and touch control routes; the touch route calls it from within
     *              enableTouchElement as it must first establish the simulated touch element to serve as the alias "input element" here.
     *              Note that the 'kmw-disabled' property is managed by the MutationObserver and by the surface API calls.
     */
    enableInputElement(Pelem: HTMLElement) {
      if(!this.isKMWDisabled(Pelem)) {
        if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
          this._AttachToIframe(Pelem);
        } else {
          this.setupElementAttachment(Pelem);

          Pelem._kmwAttachment.inputMode = Pelem.inputMode ?? 'text';
          this.disableInputModeObserver();
          Pelem.inputMode = 'none';
          this.enableInputModeObserver();

          Pelem.classList.add('keymanweb-font');
          this.inputList.push(Pelem);

          this.keyman.util.attachDOMEvent(Pelem,'focus', this.getHandlers(Pelem)._ControlFocus);
          this.keyman.util.attachDOMEvent(Pelem,'blur', this.getHandlers(Pelem)._ControlBlur);
          this.keyman.util.attachDOMEvent(Pelem,'click', this.getHandlers(Pelem)._Click);

          // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
          Pelem.onkeypress = this.getHandlers(Pelem)._KeyPress;
          Pelem.onkeydown = this.getHandlers(Pelem)._KeyDown;
          Pelem.onkeyup = this.getHandlers(Pelem)._KeyUp;
        }
      }
    };

    /**
     * Function     disableInputElement
     * Scope        Private
     * @param       {Element}   Pelem   An element from the document to be enabled with full KMW handling.
     * @param       {boolean=}   isAlias A flag that indicates if the element is a simulated input element for touch.
     * Description  Inverts the process of enableInputElement, removing all event-handling from the element.
     *              Note that the 'kmw-disabled' property is managed by the MutationObserver and by the surface API calls.
     */
    disableInputElement(Pelem: HTMLElement, isAlias?: boolean) {
      if(!Pelem) {
        return;
      }

      // Do NOT test for pre-disabledness - we also use this to fully detach without officially 'disabling' via kmw-disabled.
      if((Pelem.ownerDocument.defaultView && Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) ||
          Pelem instanceof HTMLIFrameElement) {
        this._DetachFromIframe(Pelem);
      } else {
        let cnIndex = Pelem.className.indexOf('keymanweb-font');
        if(cnIndex >= 0 && !isAlias) { // See note about the alias below.
          Pelem.className = Pelem.className.replace('keymanweb-font', '').trim();
        }

        // Remove the element from our internal input tracking.
        var index = this.inputList.indexOf(Pelem);
        if(index > -1) {
          this.inputList.splice(index, 1);
        }

        this.keyman.util.detachDOMEvent(Pelem,'focus', this.getHandlers(Pelem)._ControlFocus);
        this.keyman.util.detachDOMEvent(Pelem,'blur', this.getHandlers(Pelem)._ControlBlur);
        this.keyman.util.detachDOMEvent(Pelem,'click', this.getHandlers(Pelem)._Click);

        Pelem.onkeypress = null;
        Pelem.onkeydown = null;
        Pelem.onkeyup = null;
      }

      var lastElem = this.lastActiveElement;
      if(lastElem == Pelem) {
        if(this.activeElement == lastElem) {
          this.activeElement = null;
        }
        this.lastActiveElement = null;
        this.keyman.osk.startHide(false);
      }

      return;
    };

    /**
     * Function     isKMWDisabled
     * Scope        Private
     * @param       {Element}   x   An element from the page.
     * @return      {boolean}      true if the element's properties indicate a 'disabled' state.
     * Description  Examines attachable elements to determine their default enablement state.
     */
    isKMWDisabled(x: HTMLElement): boolean {
      var c = x.className;

      // Exists for some HTMLElements.
      if(x['readOnly']) {
        return true;
      } else if(c && c.indexOf('kmw-disabled') >= 0) {
        return true;
      }

      return false;
    }

    /**
     * Function     attachToControl
     * Scope        Public
     * @param       {Element}    Pelem       Element to which KMW will be attached
     * Description  Attaches KMW to control (or IFrame)
     */
    attachToControl(Pelem: HTMLElement) {
      var touchable = this.keyman.util.device.touchable;

      // Exception for IFrame elements, in case of async loading issues.  (Fixes fun iframe loading bug with Chrome.)
      if(this.isAttached(Pelem) && !(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement)) {
        return; // We're already attached.
      }

      if(this.isKMWInput(Pelem)) {
        if(!this.isKMWDisabled(Pelem)) {
          if(touchable && !this.keyman.isEmbedded) {
            this.enableTouchElement(Pelem);
          } else {
            this.enableInputElement(Pelem);
          }
        } else {
          if(touchable) {
            this.setupNonKMWTouchElement(Pelem);
          }
        }
      } else if(touchable) {
        this.setupNonKMWTouchElement(Pelem);
      }
    }

    /**
     * Function     detachFromControl
     * Scope        Public
     * @param       {Element}    Pelem       Element from which KMW will detach
     * Description  Detaches KMW from a control (or IFrame)
     */
    detachFromControl(Pelem: HTMLElement) {
      if(!(this.isAttached(Pelem) || Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement)) {
        return;  // We never were attached.
      }

      // #1 - if element is enabled, disable it.  But don't manipulate the 'kmw-disabled' tag.
      if(this.isKMWInput(Pelem)) {
        // Is it already disabled?
        if(!this.isKMWDisabled(Pelem)) {
          this._DisableControl(Pelem);
        }
      }

      // #2 - clear attachment data.
      this.clearElementAttachment(Pelem);
    }

    /**
     * Function     isAttached
     * Scope        Private
     * @param       {Element}   x   An element from the page.
     * @return      {boolean}       true if KMW is attached to the element, otherwise false.
     */
    isAttached(x: HTMLElement) {
      return x._kmwAttachment ? true : false;
    }

    /**
     * Function     isKMWInput
     * Scope        Private
     * @param       {Element}   x   An element from the page.
     * @return      {boolean}      true if the element is viable for KMW attachment.
     * Description  Examines potential input elements to determine whether or not they are viable for KMW attachment.
     *              Also filters elements not supported for touch devices when device.touchable == true.
     */
    isKMWInput(x: HTMLElement): boolean {
      if(x instanceof x.ownerDocument.defaultView.HTMLTextAreaElement) {
        return true;
      } else if(x instanceof x.ownerDocument.defaultView.HTMLInputElement) {
        if (x.type == 'text' || x.type == 'search') {
          return true;
        }
      } else if(x instanceof x.ownerDocument.defaultView.HTMLIFrameElement) {
        try {
          if(x.contentWindow) {
            const iframeDoc = x.contentWindow.document;
            if(iframeDoc) { // Only allow attachment if the iframe's internal document is valid.
              // Do not allow design-mode iframe attachment if in 'touch' mode.
              if(this.keyman.util.device.touchable && iframeDoc.designMode.toLowerCase() == 'on') {
                return false;
              }
              return true;
            }
          } // else nothing?
        }
        catch(err) {
          /* Do not attempt to access iframes outside this site */
          console.warn("Error during attachment to / detachment from iframe: ");
          console.warn(err);
        }
      } else if(x.isContentEditable) {
        return true;
      }

      return false;
    }

    /**
     * Function     setupElementAttachment
     * Scope        Private
     * @param       {Element}   x   An element from the page valid for KMW attachment
     * Description  Establishes the base KeymanWeb data for newly-attached elements.
     *              Does not establish input hooks, which are instead handled during enablement.
     */
    setupElementAttachment(x: HTMLElement) {
      // The `_kmwAttachment` property tag maintains all relevant KMW-maintained data regarding the element.
      // It is disgarded upon de-attachment.
      if(x._kmwAttachment) {
        return;
      } else {
        // Problem:  tries to wrap IFrames that aren't design-mode.
        // The elements in the contained document get separately wrapped, so this doesn't need a proper wrapper.
        //
        // Its attachment process might need some work.
        let eleInterface = dom.targets.wrapElement(x);
        // May should filter better for IFrames.
        if(!(eleInterface || dom.Utils.instanceof(x, "HTMLIFrameElement"))) {
          console.warn("Could not create processing interface for newly-attached element!");
        }

        x._kmwAttachment = new AttachmentInfo(eleInterface, null, this.keyman.util.device.touchable);
      }
    }

    /**
     * Function     clearElementAttachment
     * Scope        Private
     * @param       {Element}   x   An element from the page valid for KMW attachment
     * Description  Establishes the base KeymanWeb data for newly-attached elements.
     *              Does not establish input hooks, which are instead handled during enablement.
     */
    clearElementAttachment(x: HTMLElement) {
      // We need to clear the object when de-attaching; helps prevent memory leaks.
      x._kmwAttachment = null;
    }

    /**
     * Function     _AttachToIframe
     * Scope        Private
     * @param       {Element}      Pelem       IFrame to which KMW will be attached
     * Description  Attaches KeymanWeb to IFrame
     */
    _AttachToIframe(Pelem: HTMLIFrameElement) {
      var util = this.keyman.util;

      try {
        var Lelem=Pelem.contentWindow.document;
        /* editable Iframe */
        if(Lelem) {
          if(Lelem.designMode.toLowerCase() == 'on') {
            // I2404 - Attach to IFRAMEs child objects, only editable IFRAMEs here
            if(util.device.browser == 'firefox') {
              util.attachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
              util.attachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
            } else { // Chrome, Safari
              util.attachDOMEvent(Lelem.body,'focus', this.getHandlers(Pelem)._ControlFocus);
              util.attachDOMEvent(Lelem.body,'blur', this.getHandlers(Pelem)._ControlBlur);
            }
            util.attachDOMEvent(Lelem.body,'keydown', this.getHandlers(Pelem)._KeyDown);
            util.attachDOMEvent(Lelem.body,'keypress', this.getHandlers(Pelem)._KeyPress);
            util.attachDOMEvent(Lelem.body,'keyup', this.getHandlers(Pelem)._KeyUp);

            // Set up a reference alias; the internal document will need the same attachment info!
            this.setupElementAttachment(Pelem);
            Lelem.body._kmwAttachment = Pelem._kmwAttachment;
          } else {
            // Lelem is the IFrame's internal document; set 'er up!
            this._SetupDocument(Lelem.body);	   // I2404 - Manage IE events in IFRAMEs
          }
        }
      }
      catch(err)
      {
        // do not attempt to attach to the iframe as it is from another domain - XSS denied!
      }
    }

    /**
     * Function     _DetachFromIframe
     * Scope        Private
     * @param       {Element}      Pelem       IFrame to which KMW will be attached
     * Description  Detaches KeymanWeb from an IFrame
     */
    _DetachFromIframe(Pelem: HTMLIFrameElement) {
      var util = this.keyman.util;

      try {
        var Lelem=Pelem.contentWindow.document;
        /* editable Iframe */
        if(Lelem) {
          if(Lelem.designMode.toLowerCase() == 'on') {
            // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
            if(util.device.browser == 'firefox') {
              // Firefox won't handle these events on Lelem.body - only directly on Lelem (the doc) instead.
              util.detachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
              util.detachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
            } else { // Chrome, Safari
              util.detachDOMEvent(Lelem.body,'focus', this.getHandlers(Pelem)._ControlFocus);
              util.detachDOMEvent(Lelem.body,'blur', this.getHandlers(Pelem)._ControlBlur);
            }
            util.detachDOMEvent(Lelem.body,'keydown', this.getHandlers(Pelem)._KeyDown);
            util.detachDOMEvent(Lelem.body,'keypress', this.getHandlers(Pelem)._KeyPress);
            util.detachDOMEvent(Lelem.body,'keyup', this.getHandlers(Pelem)._KeyUp);

            // Remove the reference to our prior attachment data!
            Lelem.body._kmwAttachment = null;
          } else {
            // Lelem is the IFrame's internal document; set 'er up!
            this._ClearDocument(Lelem.body);	   // I2404 - Manage IE events in IFRAMEs
          }
        }
      }
      catch(err)
      {
        // do not attempt to attach to the iframe as it is from another domain - XSS denied!
      }
    }

    /**
     * Function     _GetDocumentEditables
     * Scope        Private
     * @param       {Element}     Pelem     HTML element
     * @return      {Array<Element>}        A list of potentially-editable controls.  Further filtering [as with isKMWInput() and
     *                                      isKMWDisabled()] is required.
     */
    _GetDocumentEditables(Pelem: HTMLElement): (HTMLElement)[] {
      var util = this.keyman.util;

      var possibleInputs: (HTMLElement)[] = [];

      // Document.ownerDocument === null, so we better check that it's not null before proceeding.
      if(Pelem.ownerDocument && Pelem instanceof Pelem.ownerDocument.defaultView.HTMLElement) {
        var dv = Pelem.ownerDocument.defaultView;

        if(Pelem instanceof dv.HTMLInputElement || Pelem instanceof dv.HTMLTextAreaElement) {
          possibleInputs.push(Pelem);
        } else if(Pelem instanceof dv.HTMLIFrameElement) {
          possibleInputs.push(Pelem);
        }
      }

      // Constructing it like this also allows for individual element filtering for the auto-attach MutationObserver without errors.
      if(Pelem.getElementsByTagName) {
        /**
         * Function     LiTmp
         * Scope        Private
         * @param       {string}    _colon    type of element
         * @return      {Array<Element>}  array of elements of specified type
         * Description  Local function to get list of editable controls
         */
        var LiTmp = function(_colon: string): HTMLElement[] {
          return util.arrayFromNodeList(Pelem.getElementsByTagName(_colon));
        };

        // Note that isKMWInput() will block IFRAME elements as necessary for touch-based devices.
        possibleInputs = possibleInputs.concat(LiTmp('INPUT'), LiTmp('TEXTAREA'), LiTmp('IFRAME'));
      }

      // Not all active browsers may support the method, but only those that do would work with contenteditables anyway.
      if(Pelem.querySelectorAll) {
        possibleInputs = possibleInputs.concat(util.arrayFromNodeList(Pelem.querySelectorAll('[contenteditable]')));
      }

      if(Pelem.ownerDocument && Pelem instanceof Pelem.ownerDocument.defaultView.HTMLElement && Pelem.isContentEditable) {
        possibleInputs.push(Pelem);
      }

      return possibleInputs;
    }

    /**
     * Function     _SetupDocument
     * Scope        Private
     * @param       {Element}     Pelem - the root element of a document, including IFrame documents.
     * Description  Used to automatically attach KMW to editable controls, regardless of control path.
     */
    _SetupDocument(Pelem: HTMLElement) { // I1961
      var possibleInputs = this._GetDocumentEditables(Pelem);

      for(var Li = 0; Li < possibleInputs.length; Li++) {
        var input = possibleInputs[Li];

        // It knows how to handle pre-loaded iframes appropriately.
        this.attachToControl(possibleInputs[Li] as HTMLElement);
      }
    }

    /**
     * Function     _ClearDocument
     * Scope        Private
     * @param       {Element}     Pelem - the root element of a document, including IFrame documents.
     * Description  Used to automatically detach KMW from editable controls, regardless of control path.
     *              Mostly used to clear out all controls of a detached IFrame.
     */
    _ClearDocument(Pelem: HTMLElement) { // I1961
      var possibleInputs = this._GetDocumentEditables(Pelem);

      for(var Li = 0; Li < possibleInputs.length; Li++) {
        var input = possibleInputs[Li];

        // It knows how to handle pre-loaded iframes appropriately.
        this.detachFromControl(possibleInputs[Li] as HTMLElement);
      }
    }

    /**
     * Set target element text direction (LTR or RTL), but only if the element is empty
     *
     * If the element base directionality is changed after it contains content, unless all the text
     * has the same directionality, text runs will be re-ordered which is confusing and causes
     * incorrect caret positioning
     *
     * @param       {Object}      Ptarg      Target element
     */
    _SetTargDir(Ptarg: HTMLElement) {
      let activeKeyboard = com.keyman.singleton.core.activeKeyboard;
      var elDir=(activeKeyboard && activeKeyboard.isRTL) ? 'rtl' : 'ltr';

      if(Ptarg) {
        if(Ptarg instanceof Ptarg.ownerDocument.defaultView.HTMLInputElement
            || Ptarg instanceof Ptarg.ownerDocument.defaultView.HTMLTextAreaElement) {
          if((Ptarg as HTMLInputElement|HTMLTextAreaElement).value.length == 0) {
            Ptarg.dir=elDir;
          }
        } else if(typeof Ptarg.textContent == "string" && Ptarg.textContent.length == 0) { // As with contenteditable DIVs, for example.
          Ptarg.dir=elDir;
        }
      }
    }

    /**
     * Function     _DisableControl
     * Scope        Private
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disable KMW control element
     */
    _DisableControl(Pelem: HTMLElement) {
      // Only operate on attached elements!  Non-design-mode IFrames don't get attachment markers, so we check them specifically instead.
      if(this.isAttached(Pelem) || Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
        if(this.keyman.util.device.touchable) {
          this.disableTouchElement(Pelem);
        }

        this.listInputs(); // Fix up our internal input ordering scheme.
        this.disableInputElement(Pelem);
      }
    }

    /**
     * Function     _EnableControl
     * Scope        Private
     * @param       {Element}    Pelem   Element to be enabled
     * Description  Enable KMW control element
     */
    _EnableControl(Pelem: HTMLElement) {
      if(this.isAttached(Pelem)) { // Only operate on attached elements!
        if(this.keyman.util.device.touchable) {
          this.enableTouchElement(Pelem);

          var keyman = this.keyman;

          // If we just added a new input alias, some languages will mess up our touch-based layout scheme
          // if we don't update the touch elements.
          window.setTimeout(function() {
            keyman.domManager.listInputs();
          }.bind(this), 1);
        } else {
          this.enableInputElement(Pelem);
        }
      }
    }

    // Create an ordered list of all text and search input elements and textarea elements
    // except any tagged with class 'kmw-disabled'
    // TODO: email and url types should perhaps use default keyboard only
    listInputs() {
      var i,eList=[],
        t1=document.getElementsByTagName<'input'>('input'),
        t2=document.getElementsByTagName<'textarea'>('textarea');

      var util = this.keyman.util;

      for(i=0; i<t1.length; i++) {
        switch(t1[i].type) {
          case 'text':
          case 'search':
          case 'email':
          case 'url':
            if(t1[i].className.indexOf('kmw-disabled') < 0) {
              eList.push({ip:t1[i], x: dom.Utils.getAbsoluteX(t1[i]), y: dom.Utils.getAbsoluteY(t1[i])});
            }
            break;
        }
      }

      for(i=0; i<t2.length; i++) {
        if(t2[i].className.indexOf('kmw-disabled') < 0)
          eList.push({ip:t2[i], x: dom.Utils.getAbsoluteX(t2[i]), y: dom.Utils.getAbsoluteY(t2[i])});
      }

      /**
       * Local function to sort by screen position
       *
       * @param       {Object}     e1     first object
       * @param       {Object}     e2     second object
       * @return      {number}            y-difference between object positions, or x-difference if y values the same
       */
      var xySort=function(e1,e2)
      {
        if(e1.y != e2.y) return e1.y-e2.y;
        return e1.x-e2.x;
      }

      // Sort elements by Y then X
      eList.sort(xySort);

      // Create a new list of sorted elements
      var tList=[];
      for(i=0;i<eList.length;i++)
        tList.push(eList[i].ip);

      // Return the sorted element list
      this.sortedInputs=tList;
    }

    _EnablementMutationObserverCore = function(mutations: MutationRecord[]) {
      for(var i=0; i < mutations.length; i++) {
        var mutation = mutations[i];

        // ( ? : ) needed as a null check.
        var disabledBefore = mutation.oldValue ? mutation.oldValue.indexOf('kmw-disabled') >= 0 : false;
        var disabledAfter = (mutation.target as HTMLElement).className.indexOf('kmw-disabled') >= 0;

        if(disabledBefore && !disabledAfter) {
          this._EnableControl(mutation.target);
        } else if(!disabledBefore && disabledAfter) {
          this._DisableControl(mutation.target);
        }

        // 'readonly' triggers on whether or not the attribute exists, not its value.
        if(!disabledAfter && mutation.attributeName == "readonly") {
          var readonlyBefore = mutation.oldValue ? mutation.oldValue != null : false;
          var elem = mutation.target;

          if(elem instanceof elem.ownerDocument.defaultView.HTMLInputElement
              || elem instanceof elem.ownerDocument.defaultView.HTMLTextAreaElement) {
            var readonlyAfter = elem.readOnly;

            if(readonlyBefore && !readonlyAfter) {
              this._EnableControl(mutation.target);
            } else if(!readonlyBefore && readonlyAfter) {
              this._DisableControl(mutation.target);
            }
          }
        }
      }
    }.bind(this);

    _AutoAttachObserverCore = function(mutations: MutationRecord[]) {
      var inputElementAdditions = [];
      var inputElementRemovals = [];

      for(var i=0; i < mutations.length; i++) {
        var mutation = mutations[i];

        for(var j=0; j < mutation.addedNodes.length; j++) {
          inputElementAdditions = inputElementAdditions.concat(this._GetDocumentEditables(mutation.addedNodes[j]));
        }

        for(j = 0; j < mutation.removedNodes.length; j++) {
          inputElementRemovals = inputElementRemovals.concat(this._GetDocumentEditables(mutation.removedNodes[j]));
        }
      }

      for(var k = 0; k < inputElementAdditions.length; k++) {
        if(this.isKMWInput(inputElementAdditions[k])) { // Apply standard element filtering!
          this._MutationAdditionObserved(inputElementAdditions[k]);
        }
      }

      for(k = 0; k < inputElementRemovals.length; k++) {
        if(this.isKMWInput(inputElementRemovals[k])) { // Apply standard element filtering!
          this._MutationRemovalObserved(inputElementRemovals[k]);
        }
      }

      /* After all mutations have been handled, we need to recompile our .sortedInputs array, but only
        * if any have actually occurred.
        */
      if(inputElementAdditions.length || inputElementRemovals.length) {
        if(!this.keyman.util.device.touchable) {
          this.listInputs();
        } else if(this.keyman.util.device.touchable) {   // If something was added or removed, chances are it's gonna mess up our touch-based layout scheme, so let's update the touch elements.
          var domManager = this;
          window.setTimeout(function() {
            domManager.listInputs();
          }.bind(this), 1);
        }
      }
    }.bind(this);

    /**
     * The core method for a MutationObserver that checks for changes to the `.inputMode` property
     * of controls that KMW is attached to in touch mode.
     *
     * In touch mode, KMW requires that their `.inputMode` property be set to 'none' in order
     * to hide the device's default OSK.  That said, we should still aim to honor the setting
     * and restore it if and when detachment occurs.  Should we ever support intents, we'll want
     * to utilize the incoming value for use with that feature too.
     */
    _InputModeObserverCore = function(this: DOMManager, mutations: MutationRecord[]) {
      const keyman = com.keyman.singleton;
      // Prevent infinite recursion from any changes / updates made within the observation handler.
      this.disableInputModeObserver();
      try {
        for(const mutation of mutations) {
          const target = mutation.target as HTMLElement;
          if(!this.isAttached(target)) {
            continue;
          }

          target._kmwAttachment.inputMode = target.inputMode;

          if(keyman.util.device.touchable) {
            target.inputMode = 'none';
          }
        }
      } finally {
        this.enableInputModeObserver();
      }
    }.bind(this);

    /**
     * Function     _MutationAdditionObserved
     * Scope        Private
     * @param       {Element}  Pelem     A page input, textarea, or iframe element.
     * Description  Used by the MutationObserver event handler to properly setup any elements dynamically added to the document post-initialization.
     *
     */
    _MutationAdditionObserved = function(Pelem: HTMLElement) {
      if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
        //Problem:  the iframe is loaded asynchronously, and we must wait for it to load fully before hooking in.

        var domManager = this;

        var attachFunctor = function() {  // Triggers at the same time as iframe's onload property, after its internal document loads.
          // Provide a minor delay to allow 'load' event handlers to set the design-mode property.
          window.setTimeout(function() {
            domManager.attachToControl(Pelem);
          }, 1);
        };

        Pelem.addEventListener('load', attachFunctor);

        // The following block breaks for design-mode iframes, at least in Chrome; a blank document may exist
        // before the load of the desired actual document.
        //
        // /* If the iframe has somehow already loaded, we can't expect the onload event to be raised.  We ought just
        // * go ahead and perform our callback's contents.
        // *
        // * keymanweb.domManager.attachToControl() is now idempotent, so even if our call 'whiffs', it won't cause long-lasting
        // * problems.
        // */
        // if(Pelem.contentDocument.readyState == 'complete') {
        //   window.setTimeout(attachFunctor, 1);
        // }
      } else {
        this.attachToControl(Pelem);
      }
    }

    // Used by the mutation event handler to properly decouple any elements dynamically removed from the document.
    _MutationRemovalObserved = function(Pelem: HTMLElement) {
      var element = Pelem;
      if(this.keyman.util.device.touchable) {
        this.disableTouchElement(Pelem);
      }

      this.disableInputElement(Pelem); // Remove all KMW event hooks, styling.
      this.clearElementAttachment(element);  // Memory management & auto de-attachment upon removal.
    }

    /**
     * Function     disableControl
     * Scope        Public
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disables a KMW control element
     */
    disableControl(Pelem: HTMLElement) {
      if(!this.isAttached(Pelem)) {
        console.warn("KeymanWeb is not attached to element " + Pelem);
      }

      var cn = Pelem.className;
      if(cn.indexOf('kmw-disabled') < 0) { // if not already explicitly disabled...
        Pelem.className = cn ? cn + ' kmw-disabled' : 'kmw-disabled';
      }

      // The rest is triggered within MutationObserver code.
      // See _EnablementMutationObserverCore.
    }

    /**
     * Function     enableControl
     * Scope        Public
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disables a KMW control element
     */
    enableControl = function(Pelem: HTMLElement) {
      if(!this.isAttached(Pelem)) {
        console.warn("KeymanWeb is not attached to element " + Pelem);
      }

      var cn = Pelem.className;
      var tagIndex = cn.indexOf('kmw-disabled');
      if(tagIndex >= 0) { // if already explicitly disabled...
        Pelem.className = cn.replace('kmw-disabled', '').trim();
      }

      // The rest is triggered within MutationObserver code.
      // See _EnablementMutationObserverCore.
    }

    /* ------------- Page and document-level management events ------------------ */

    _WindowLoad: (e: Event) => void = function(e: Event) {
      //keymanweb.completeInitialization();
      // Always return to top of page after a page reload
      document.body.scrollTop=0;
      if(typeof document.documentElement != 'undefined') {
        document.documentElement.scrollTop=0;
      }
    }.bind(this);

    /**
     * Function     _WindowUnload
     * Scope        Private
     * Description  Remove handlers before detaching KMW window
     */
    _WindowUnload: () => void = function(this: DOMManager) {
      // Allow the UI to release its own resources
      this.keyman.uiManager.doUnload();

      // Allow the OSK to release its own resources
      if(this.keyman.osk) {
        this.keyman.osk.shutdown();
        if(this.keyman.osk['_Unload']) {
          this.keyman.osk['_Unload'](); // I3363 (Build 301)
        }
      }

      this.lastActiveElement = null;
    }.bind(this);

    /* ------ Defines independent, per-control keyboard setting behavior for the API. ------ */

    /**
     * Function     setKeyboardForControl
     * Scope        Public
     * @param       {Element}    Pelem    Control element
     * @param       {string|null=}    Pkbd     Keyboard (Clears the set keyboard if set to null.)
     * @param       {string|null=}     Plc      Language Code
     * Description  Set default keyboard for the control
     */
    setKeyboardForControl(Pelem: HTMLElement, Pkbd?: string, Plc?: string) {
      if(!Pelem) {
        return;
      }

      /* pass null for kbd to specify no default, or '' to specify the default system keyboard. */
      if(Pkbd !== null && Pkbd !== undefined) {
        var index = Pkbd.indexOf("Keyboard_");
        if(index < 0 && Pkbd != '') {
          Pkbd = "Keyboard_" + Pkbd;
        }
      } else {
        Plc = null;
      }

      if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
        console.warn("'keymanweb.setKeyboardForControl' cannot set keyboard on iframes.");
        return;
      }

      if(!this.isAttached(Pelem)) {
        console.error("KeymanWeb is not attached to element " + Pelem);
        return;
      } else {
        Pelem._kmwAttachment.keyboard = Pkbd;
        Pelem._kmwAttachment.languageCode = Plc;

        // If Pelem is the focused element/active control, we should set the keyboard in place now.

        var lastElem = this.lastActiveElement;
        if(lastElem && (lastElem == Pelem)) {

          if(Pkbd != null && Plc != null) { // Second part necessary for Closure.
            this.keyman.keyboardManager.setActiveKeyboard(Pkbd, Plc);
          } else {
            this.keyman.keyboardManager.setActiveKeyboard(this.keyman.globalKeyboard, this.keyman.globalLanguageCode);
          }
        }
      }
    }

    /**
     * Function     getKeyboardForControl
     * Scope        Public
     * @param       {Element}    Pelem    Control element
     * @return      {string|null}         The independently-managed keyboard for the control.
     * Description  Returns the keyboard ID of the current independently-managed keyboard for this control.
     *              If it is currently following the global keyboard setting, returns null instead.
     */
    getKeyboardForControl(Pelem: HTMLElement): string {
      if(!this.isAttached(Pelem)) {
        console.error("KeymanWeb is not attached to element " + Pelem);
        return null;
      } else {
        return Pelem._kmwAttachment.keyboard;
      }
    }

    /**
     * Function     getLanguageForControl
     * Scope        Public
     * @param       {Element}    Pelem    Control element
     * @return      {string|null}         The independently-managed keyboard for the control.
     * Description  Returns the language code used with the current independently-managed keyboard for this control.
     *              If it is currently following the global keyboard setting, returns null instead.
     */
    getLanguageForControl(Pelem: HTMLElement): string {
      if(!this.isAttached(Pelem)) {
        console.error("KeymanWeb is not attached to element " + Pelem);
        return null;
      } else {
        return Pelem._kmwAttachment.languageCode;  // Should we have a version for the language code, too?
      }
    }

    /* ------ End independent, per-control keyboard setting behavior definitions. ------ */

    /**
     * Set focus to last active target element (browser-dependent)
     */
    focusLastActiveElement() {
      var lastElem = this.lastActiveElement;
      if(!lastElem) {
        return;
      }

      this.keyman.uiManager.justActivated = true;

      const target = Utils.getOutputTarget(lastElem);
      target.focus();
    }

    /**
     * Get the last active target element *before* KMW activated (I1297)
     *
     * @return      {Element}
     */
    get lastActiveElement(): HTMLElement {
      return DOMEventHandlers.states._lastActiveElement;
    }

    set lastActiveElement(Pelem: HTMLElement) {
      DOMEventHandlers.states._lastActiveElement = Pelem;

      const osk = this.keyman.osk;
      if(osk) {
        if(this.lastActiveElement == null && this.activeElement == null) {
          // Assigning to the property does have side-effects.
          // If the property is already unset, it's best to not unset it again.
          osk.activeTarget = null;
          this.keyman.osk.hideNow(); // originally from a different one, seemed to serve the same role?
        }
      }
    }

    get activeElement(): HTMLElement {
      return DOMEventHandlers.states._activeElement;
    }

    set activeElement(Pelem: HTMLElement) {
      DOMEventHandlers.states._activeElement = Pelem;

      var isActivating = this.keyman.uiManager.isActivating;

      // Hide the OSK when the control is blurred, unless the UI is being temporarily selected
      const osk = this.keyman.osk;
      // const device = this.keyman.util.device;

      if(osk) {
        const target = Pelem?._kmwAttachment?.interface || null;
        if(osk && osk.activationModel instanceof TwoStateActivator && (target || !isActivating)) {
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

    /**
     * Move focus to next (or previous) input or text area element on TAB
     *   Uses list of actual input elements
     *
     *   Note that activeElement() on touch devices returns the DIV that overlays
     *   the input element, not the element itself.
     *
     * @param      {number|boolean}  bBack     Direction to move (0 or 1)
     */
    moveToNext(bBack: number|boolean) {
      var i,t=this.sortedInputs, activeBase = this.activeElement;
      var touchable = this.keyman.util.device.touchable;

      if(t.length == 0) {
        return;
      }

      // For touchable devices, get the base element of the DIV
      if(touchable) {
        activeBase=activeBase.base;
      }

      // Identify the active element in the list of inputs ordered by position
      for(i=0; i<t.length; i++) {
        if(t[i] == activeBase) break;
      }

      // Find the next (or previous) element in the list
      i = bBack ? i-1 : i+1;
      // Treat the list as circular, wrapping the index if necessary.
      i = i >= t.length ? i-t.length : i;
      i = i < 0 ? i+t.length : i;

      // Move to the selected element
      t[i].focus();
    }

    /**
     * Move focus to user-specified element
     *
     *  @param  {string|Object}   e   element or element id
     *
     */
    moveToElement(e:string|HTMLElement) {
      if(typeof(e) == "string") { // Can't instanceof string, and String is a different type.
        e=document.getElementById(e);
      }

      e.focus();
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

      // Local function to convert relative to absolute URLs
      // with respect to the source path, server root and protocol
      var fixPath = function(p) {
        if(p.length == 0) return p;

        // Add delimiter if missing
        if(p.substr(p.length-1,1) != '/') p = p+'/';

        // Absolute
        if((p.replace(/^(http)s?:.*/,'$1') == 'http')
            || (p.replace(/^(file):.*/,'$1') == 'file'))
          return p;

        // Absolute (except for protocol)
        if(p.substr(0,2) == '//')
          return this.keyman.protocol+p;

        // Relative to server root
        if(p.substr(0,1) == '/')
          return this.keyman.rootPath+p.substr(1);

        // Otherwise, assume relative to source path
        return this.keyman.srcPath+p;
      }.bind(this);

      // Explicit (user-defined) parameter initialization
      opt=this.keyman.options;
      if(typeof(arg) == 'object' && arg !== null)
      {
        for(p in opt)
        {
          if(arg.hasOwnProperty(p)) opt[p] = arg[p];
        }
      }

      // Get default paths and device options
      if(opt['root'] != '') {
        this.keyman.rootPath = fixPath(opt['root']);
      }

      // Keyboards and fonts are located with respect to the server root by default
      //if(opt['keyboards'] == '') opt['keyboards'] = keymanweb.rootPath+'keyboard/';
      //if(opt['fonts'] == '') opt['fonts'] = keymanweb.rootPath+'font/';

      // Resources are located with respect to the engine by default
      if(opt['resources'] == '') {
        opt['resources'] = this.keyman.srcPath;
      }

      // Convert resource, keyboard and font paths to absolute URLs
      opt['resources'] = fixPath(opt['resources']);
      opt['keyboards'] = fixPath(opt['keyboards']);
      opt['fonts'] = fixPath(opt['fonts']);

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

      // Exit initialization here if we're using an embedded code path.
      if(this.keyman.isEmbedded) {
        this.keyman.keyboardManager.setDefaultKeyboard();
        return Promise.resolve();
      }

      // Determine the default font for mapped elements
      this.keyman.appliedFont=this.keyman.baseFont=this.getBaseFont();

      // Add orientationchange event handler to manage orientation changes on mobile devices
      // Initialize touch-screen device interface  I3363 (Build 301)
      if(device.touchable) {
        this.keyman.handleRotationEvents();
      }
      // Initialize browser interface

      if(this.keyman.options['attachType'] != 'manual') {
        this._SetupDocument(document.documentElement);
      }

      // Create an ordered list of all input and textarea fields
      this.listInputs();

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

        // Add a blank DIV to the bottom of the page to allow the bottom of the page to be shown
        dTrailer=document.createElement('DIV');
        ds=dTrailer.style;
        ds.width='100%';
        ds.height=(screen.width/2)+'px';
        document.body.appendChild(dTrailer);

        // Sets up page-default touch-based handling for activation-state management.
        // These always trigger for the page, wherever a touch may occur. Does not
        // prevent element-specific or OSK-key-specific handling from triggering.
        const _this = this;
        this.touchStartActivationHandler=function(e) {
          _this.deactivateOnRelease=true;
          _this.touchY=e.touches[0].screenY;

          // On Chrome, scrolling up or down causes the URL bar to be shown or hidden
          // according to whether or not the document is at the top of the screen.
          // But when doing that, each OSK row top and height gets modified by Chrome
          // looking very ugly.  It would be best to hide the OSK then show it again
          // when the user scroll finishes, but Chrome has no way to reliably report
          // the touch end event after a move. c.f. http://code.google.com/p/chromium/issues/detail?id=152913
          // The best compromise behaviour is simply to hide the OSK whenever any
          // non-input and non-OSK element is touched.
          _this.deactivateOnScroll=false;
          if(device.OS == 'Android' && navigator.userAgent.indexOf('Chrome') > 0) {
            // _this.deactivateOnScroll has the inverse of the 'true' default,
            // but that fact actually facilitates the following conditional logic.
            if(typeof(osk._Box) == 'undefined') return false;
            if(typeof(osk._Box.style) == 'undefined') return false;

            // The following tests are needed to prevent the OSK from being hidden during normal input!
            let p=(e.target as HTMLElement).parentElement;
            if(typeof(p) != 'undefined' && p != null) {
              if(p.className.indexOf('kmw-key-') >= 0) return false;
              if(typeof(p.parentElement) != 'undefined' && p.parentElement != null) {
                p=p.parentElement;
                if(p.className.indexOf('kmw-key-') >= 0) return false;
              }
            }

            _this.deactivateOnScroll = true;
          }
          return false;
        };
        this.touchMoveActivationHandler = function(e) {
          if(_this.deactivateOnScroll) {  // Android / Chrone case.
            DOMEventHandlers.states.focusing = false;
            _this.activeElement = null;
          }

          const y = e.touches[0].screenY;
          const y0 = _this.touchY;
          if(y-y0 > 5 || y0-y < 5) {
            _this.deactivateOnRelease = false;
          }
          return false;
        };
        this.touchEndActivationHandler = function() {
          // Should not hide OSK if simply closing the language menu (30/4/15)
          // or if the focusing timer (setFocusTimer) is still active.
          if(_this.deactivateOnRelease && !osk['lgList'] && !DOMEventHandlers.states.focusing) {
            _this.activeElement = null;
          }
          _this.deactivateOnRelease=false;
          return false;
        };

        this.keyman.util.attachDOMEvent(document.body, 'touchstart', this.touchStartActivationHandler,false);
        this.keyman.util.attachDOMEvent(document.body, 'touchmove',  this.touchMoveActivationHandler, false);
        this.keyman.util.attachDOMEvent(document.body, 'touchend',   this.touchEndActivationHandler,  false);
      }

      //document.body.appendChild(keymanweb._StyleBlock);

      // Restore and reload the currently selected keyboard, selecting a default keyboard if necessary.
      this.keyman.keyboardManager.restoreCurrentKeyboard();

      /* Setup of handlers for dynamically-added and (eventually) dynamically-removed elements.
        * Reference: https://developer.mozilla.org/en/docs/Web/API/MutationObserver
        *
        * We place it here so that it loads after most of the other UI loads, reducing the MutationObserver's overhead.
        * Of course, we only want to dynamically add elements if the user hasn't enabled the manual attachment option.
        */

      if(typeof MutationObserver == 'function') {
        var observationTarget = document.querySelector('body'), observationConfig: MutationObserverInit;
        if(this.keyman.options['attachType'] != 'manual') { //I1961
          observationConfig = { childList: true, subtree: true};
          this.attachmentObserver = new MutationObserver(this._AutoAttachObserverCore);
          this.attachmentObserver.observe(observationTarget, observationConfig);
        }

        /*
         * Setup of handlers for dynamic detection of the kmw-disabled class tag that controls enablement.
         */
        observationConfig = { subtree: true, attributes: true, attributeOldValue: true, attributeFilter: ['class', 'readonly']};
        this.enablementObserver = new MutationObserver(this._EnablementMutationObserverCore);
        this.enablementObserver.observe(observationTarget, observationConfig);

        /**
         * Setup of handlers for dynamic detection of change in inputMode state.
         */
        this.inputModeObserver = new MutationObserver(this._InputModeObserverCore);
        this.enableInputModeObserver();
      } else {
        console.warn("Your browser is outdated and does not support MutationObservers, a web feature " +
          "needed by KeymanWeb to support dynamically-added elements.");
      }

      // Set exposed initialization flag to 2 to indicate deferred initialization also complete

      /* To prevent propagation of focus & blur events from the input-scroll workaround,
       * we attach top-level capturing listeners to the focus & blur events.  They prevent propagation
       * but NOT default behavior, allowing the scroll to complete while preventing nearly all
       * possible event 'noise' that could result from the workaround.
       */
      this.keyman.util.attachDOMEvent(document.body, 'focus', DOMManager.suppressFocusCheck, true);
      this.keyman.util.attachDOMEvent(document.body, 'blur', DOMManager.suppressFocusCheck, true);

      this.keyman.setInitialized(2);
      return Promise.resolve();
    }.bind(this);

  /**
   *  Add or replace the style sheet used to set the font for input elements
   *
   *  @param  {Object}  kfd   KFont font descriptor
   *  @return {string}
   *
   **/
  setAttachmentFontStyle(keyboardFontDescriptor /* : KeyboardFont */): string {
    let kfd = keyboardFontDescriptor;

    // Get name of font to be applied
    var fontName = this.keyman.baseFont;
    if (typeof (kfd) != 'undefined' && typeof (kfd['family']) != 'undefined') {
      fontName = kfd['family']; // If we have a font set by the keyboard, prioritize that over the base font.
    }

    // Unquote font name in base font (if quoted)
    fontName = fontName.replace(/\u0022/g, '');

    // Set font family chain for mapped elements and remove any double quotes
    // font-family:  maintains the base font as a fallback.
    var rx = new RegExp('\\s?' + fontName + ',?'), fontFamily = this.keyman.appliedFont.replace(/\u0022/g, '');

    // Remove base font name from chain if present
    fontFamily = fontFamily.replace(rx, '');
    fontFamily = fontFamily.replace(/,$/, '');

    // Then replace it at the head of the chain
    if (fontFamily == '') {
      fontFamily = fontName;
    } else {
      fontFamily = fontName + ',' + fontFamily;
    }

    // Re-insert quotes around individual font names
    fontFamily = '"' + fontFamily.replace(/\,\s?/g, '","') + '"';

    // Add to the stylesheet, quoted, and with !important to override any explicit style
    var s = '.keymanweb-font{\nfont-family:' + fontFamily + ' !important;\n}\n';

    // Store the current font chain (with quote-delimited font names)
    this.keyman.appliedFont = fontFamily;

    // Return the style string
    return s;
  }

    enableInputModeObserver() {
      const observationTarget = document.querySelector('body');
      const observationConfig = { subtree: true, attributes: true, attributeFilter: ['inputmode'] };
      this.inputModeObserver?.observe(observationTarget, observationConfig);
    }

    disableInputModeObserver() {
      this.inputModeObserver?.disconnect();
    }

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

    static suppressFocusCheck(e: Event) {
      if(DOMEventHandlers.states._IgnoreBlurFocus) {
        // Prevent triggering other blur-handling events (as possible)
        e.stopPropagation();
        e.cancelBubble = true;
      }
      // But DO perform default event behavior (actually blurring & focusing the affected element)
      return true;
    }
  }
}