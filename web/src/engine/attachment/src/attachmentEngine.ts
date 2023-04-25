import EventEmitter from 'eventemitter3';

import { DeviceSpec } from "@keymanapp/keyboard-processor";
import { nestedInstanceOf, wrapElement } from "keyman/engine/element-wrappers";
import { arrayFromNodeList, getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";

import { AttachmentInfo } from "./attachmentInfo.js";

// Extends the standard DOM definition for HTMLElement with our custom property underlying KMW element attachment.
declare global {
  interface HTMLElement {
    /**
     * A custom property used by Keyman Engine for Web to tag elements that it has attached to.
     */
    _kmwAttachment: AttachmentInfo,     // Used to track each input element's attachment data.
  }
}

// Used for the `.sortedList` property - that is, for ordering valid input elements based
// upon their location within the page.
type SortableInput = {
  ip: HTMLInputElement | HTMLTextAreaElement,
  x: number,
  y: number
};

interface EventMap {
  /***
   * For anything attached but (design-mode) iframes...
    ```
    // This block:  has to do with maintaining focus.
    if(touchable) {
      // Remove any handlers for "NonKMWTouch" elements, since we're enabling it here.
      Pelem.removeEventListener('touchstart', this.nonKMWTouchHandler);

      // Prevent base-page touch handlers from causing a defocus when interacting
      // with attached input elements.
      Pelem.addEventListener('touchmove', (event) => event.stopPropagation(), false);
      Pelem.addEventListener('touchend', (event) => event.stopPropagation(), false);
    }

    // This block:  has to do with maintaining focus.
    this.keyman.util.attachDOMEvent(Pelem,'focus', this.getHandlers(Pelem)._ControlFocus);
    this.keyman.util.attachDOMEvent(Pelem,'blur', this.getHandlers(Pelem)._ControlBlur);
    this.keyman.util.attachDOMEvent(Pelem,'click', this.getHandlers(Pelem)._Click);

    // This block:  has to do with keystroke processing.
    // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
    Pelem.onkeypress = this.getHandlers(Pelem)._KeyPress;
    Pelem.onkeydown = this.getHandlers(Pelem)._KeyDown;
    Pelem.onkeyup = this.getHandlers(Pelem)._KeyUp;
    ```
   *
   * For design-mode iframes:
    ```
    // This block:  has to do with maintaining focus.
    var Lelem=Pelem.contentWindow.document;  // where Pelem = obj, the provided element.
    // I2404 - Attach to IFRAMEs child objects, only editable IFRAMEs here
    if(this.device.browser == 'firefox') {
      util.attachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
      util.attachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
    } else { // Chrome, Safari
      util.attachDOMEvent(Lelem.body,'focus', this.getHandlers(Pelem)._ControlFocus);
      util.attachDOMEvent(Lelem.body,'blur', this.getHandlers(Pelem)._ControlBlur);
    }

    // This block:  has to do with keystroke processing.
    util.attachDOMEvent(Lelem.body,'keydown', this.getHandlers(Pelem)._KeyDown);
    util.attachDOMEvent(Lelem.body,'keypress', this.getHandlers(Pelem)._KeyPress);
    util.attachDOMEvent(Lelem.body,'keyup', this.getHandlers(Pelem)._KeyUp);
    ```
   */
  'enabled': (obj: HTMLElement) => void;

  /***
   * For anything attached but (design-mode) iframes...
    ```
    // This block:  has to do with maintaining focus.
    if(touchable) {
      this.keyman.util.attachDOMEvent(x, 'touchstart', this.nonKMWTouchHandler, false);

      // does not detach the touch-handlers added in 'enabled'?
    }

    // This block:  has to do with maintaining focus.
    this.keyman.util.detachDOMEvent(Pelem,'focus', this.getHandlers(Pelem)._ControlFocus);
    this.keyman.util.detachDOMEvent(Pelem,'blur', this.getHandlers(Pelem)._ControlBlur);
    this.keyman.util.detachDOMEvent(Pelem,'click', this.getHandlers(Pelem)._Click);

    // This block:  has to do with keystroke processing.
    Pelem.onkeypress = null;
    Pelem.onkeydown = null;
    Pelem.onkeyup = null;
    ```

   * also (for anything but iframes)...

    ```
    // This block:  has to do with maintaining focus (and consequences)
    var lastElem = this.lastActiveElement;
    if(lastElem == Pelem) {
      if(this.activeElement == lastElem) {
        this.activeElement = null;
      }
      this.lastActiveElement = null;
      this.keyman.osk.startHide(false);
    }
    ```
   *
   * For design-mode iframes:
    ```
    // This block:  has to do with maintaining focus.
    var Lelem=Pelem.contentWindow.document;
    // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
    if(util.device.browser == 'firefox') {
      // Firefox won't handle these events on Lelem.body - only directly on Lelem (the doc) instead.
      util.detachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
      util.detachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
    } else { // Chrome, Safari
      util.detachDOMEvent(Lelem.body,'focus', this.getHandlers(Pelem)._ControlFocus);
      util.detachDOMEvent(Lelem.body,'blur', this.getHandlers(Pelem)._ControlBlur);
    }

    // This block:  has to do with keystroke processing.
    util.detachDOMEvent(Lelem.body,'keydown', this.getHandlers(Pelem)._KeyDown);
    util.detachDOMEvent(Lelem.body,'keypress', this.getHandlers(Pelem)._KeyPress);
    util.detachDOMEvent(Lelem.body,'keyup', this.getHandlers(Pelem)._KeyUp);
    ```
   */
  'disabled': (obj: HTMLElement) => void;
}

/**
 * Given a page `Document`, this class is responsible for Keyman Engine for Web's
 * "attachment" mechanism, which is used to hook into page elements to determine
 * the user's active context and to receive any keystrokes said context receives.
 *
 * In its current form, note that this engine requires an initial reference
 * to the top-level `document.body` element in order to get started - this should
 * be passed into the _SetupDocument method.  (Named so for legacy, pre-modularization
 * reasons.)
 *
 * Note:  part of this class's design is to facilitate unit testing for the core
 * attachment algorithm - for validating the logic that determines which elements
 * gain attachment.
 */
export class AttachmentEngine extends EventEmitter<EventMap> {
  // Note:  we only seem to rely on `device.touchable` within this class?  None of the other properties.
  readonly device: DeviceSpec;

  // Only used for `shutdown`; order doesn't matter.
  private _inputList: HTMLElement[] = [];

  public get inputList(): HTMLElement[] {
    return this._inputList;
  }

  // Useful for `moveToNext` operations:  order matters.
  // Note that it only includes `input` and `textarea` elements of the top-level document!
  // Anything in embedded iframes is ignored for this.
  private _sortedInputs: HTMLElement[] = [];

  public get sortedInputs(): ReadonlyArray<HTMLElement> {
    return this._sortedInputs;
  }

  /**
   * Tracks the attachment MutationObserver.
   */
  private attachmentObserver: MutationObserver;

  /**
   * Tracks the enablement MutationObserver.
   */
  private enablementObserver: MutationObserver;

  /**
   * Tracks changes in inputmode state for attached elements.
   */
  private inputModeObserver: MutationObserver;

  constructor(device: DeviceSpec) {
    super();
    this.device = device;
  }

  init() {
    this.inputModeObserver = new MutationObserver(this._InputModeObserverCore);
    this.enableInputModeObserver();
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
      let eleInterface = wrapElement(x);

      // May should filter better for IFrames.
      if(!(eleInterface || nestedInstanceOf(x, "HTMLIFrameElement"))) {
        console.warn("Could not create processing interface for newly-attached element!");
      }

      x._kmwAttachment = new AttachmentInfo(eleInterface, null, this.device.touchable);
      this.emit('enabled', x);
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
    this.emit('disabled', x);
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
            if(this.device.touchable && iframeDoc.designMode.toLowerCase() == 'on') {
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
   * Function     isAttached
   * Scope        Private
   * @param       {Element}   x   An element from the page.
   * @return      {boolean}       true if KMW is attached to the element, otherwise false.
   */
  isAttached(x: HTMLElement) {
    return x._kmwAttachment ? true : false;
  }

  /**
   * Function     isKMWDisabled
   * Scope        Private
   * @param       {Element}   x   An element from the page.
   * @return      {boolean}      true if the element's properties indicate a 'disabled' state.
   * Description  Examines attachable elements to determine their default enablement state.
   */
  isKMWDisabled(x: HTMLElement): boolean {
    const c = x.className;

    // Exists for some HTMLElements.
    if(x['readOnly']) {
      return true;
    } else if(c && c.indexOf('kmw-disabled') >= 0) {
      return true;
    }

    return false;
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
        // ensures that the system keyboard doesn't show on mobile devices.
        Pelem.inputMode = 'none';
        this.enableInputModeObserver();

        Pelem.classList.add('keymanweb-font');
        this.inputList.push(Pelem);
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

    if(this.isAttached(Pelem)) {
      const intendedInputMode = Pelem._kmwAttachment.inputMode;

      this.disableInputModeObserver();
      // restores the last-known setting before KMW forced it to 'none'.
      // Refer to enableInputElement.
      Pelem.inputMode = intendedInputMode;
      this.enableInputModeObserver();
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
    }

    return;
  };

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
    }

    if(!this.isAttached(Pelem)) {
      this.setupElementAttachment(Pelem);
    }
    Pelem._kmwAttachment.touchEnabled = true;

    // Set font for base element
    this.enableInputElement(Pelem);

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
   * Function     setupNonKMWTouchElement
   * Scope        Private
   * @param       {Element}    x  A child element of document.
   * Description  Performs handling for the specified disabled input element on touch-based systems.
   */
  setupNonKMWTouchElement(x: HTMLElement) {
    // Signify that touch isn't enabled on the control.
    if(this.isAttached(x)) {
      x._kmwAttachment.touchEnabled = false;
    }
  }

  /**
   * The core method for a MutationObserver that checks for changes to the `.inputMode` property
   * of controls that KMW is attached to in touch mode.
   *
   * In touch mode, KMW requires that their `.inputMode` property be set to 'none' in order
   * to hide the device's default OSK.  That said, we should still aim to honor the setting
   * and restore it if and when detachment occurs.  Should we ever support intents, we'll want
   * to utilize the incoming value for use with that feature too.
   */
  private _InputModeObserverCore = (mutations: MutationRecord[]) => {
    // Prevent infinite recursion from any changes / updates made within the observation handler.
    this.disableInputModeObserver();
    try {
      for(const mutation of mutations) {
        const target = mutation.target as HTMLElement;
        if(!this.isAttached(target)) {
          continue;
        }

        target._kmwAttachment.inputMode = target.inputMode;

        if(this.device.touchable) {
          target.inputMode = 'none';
        }
      }
    } finally {
      this.enableInputModeObserver();
    }
  };

  /**
     * Function     _AttachToIframe
     * Scope        Private
     * @param       {Element}      Pelem       IFrame to which KMW will be attached
     * Description  Attaches KeymanWeb to IFrame
     */
  _AttachToIframe(Pelem: HTMLIFrameElement) {
    try {
      var Lelem=Pelem.contentWindow.document;
      /* editable Iframe */
      if(Lelem) {
        if(Lelem.designMode.toLowerCase() == 'on') {
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
    try {
      var Lelem=Pelem.contentWindow.document;
      /* editable Iframe */
      if(Lelem) {
        if(Lelem.designMode.toLowerCase() == 'on') {
          // Remove the reference to our prior attachment data!
          this.clearElementAttachment(Pelem);
          Lelem.body._kmwAttachment = null; // is an extra step needed for this case.
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
   * Function     attachToControl
   * Scope        Public
   * @param       {Element}    Pelem       Element to which KMW will be attached
   * Description  Attaches KMW to control (or IFrame)
   */
  attachToControl(Pelem: HTMLElement) {
    var touchable = this.device.touchable;

    // Exception for IFrame elements, in case of async loading issues.  (Fixes fun iframe loading bug with Chrome.)
    if(this.isAttached(Pelem) && !(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement)) {
      return; // We're already attached.
    }

    if(this.isKMWInput(Pelem)) {
      if(!this.isKMWDisabled(Pelem)) {
        if(touchable) {
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
   * Function     _DisableControl
   * Scope        Private
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disable KMW control element
   */
  _DisableControl(Pelem: HTMLElement) {
    // Only operate on attached elements!  Non-design-mode IFrames don't get attachment markers, so we check them specifically instead.
    if(this.isAttached(Pelem) || Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
      if(this.device.touchable) {
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
      if(this.device.touchable) {
        this.enableTouchElement(Pelem);

      } else {
        this.enableInputElement(Pelem);
      }

      this.listInputs();
    }
  }

  // Create an ordered list of all text and search input elements and textarea elements
  // except any tagged with class 'kmw-disabled'
  // TODO: email and url types should perhaps use default keyboard only
  listInputs() {
    let eList: SortableInput[]=[];
    let t1=document.getElementsByTagName('input');
    let t2=document.getElementsByTagName('textarea');

    for(let i=0; i<t1.length; i++) {
      switch(t1[i].type) {
        case 'text':
        case 'search':
        case 'email':
        case 'url':
          if(t1[i].className.indexOf('kmw-disabled') < 0) {
            eList.push({ip:t1[i], x: getAbsoluteX(t1[i]), y: getAbsoluteY(t1[i])});
          }
          break;
      }
    }

    for(let i=0; i<t2.length; i++) {
      if(t2[i].className.indexOf('kmw-disabled') < 0) {
        eList.push({ip:t2[i], x: getAbsoluteX(t2[i]), y: getAbsoluteY(t2[i])});
      }
    }

    // Sort elements by Y then X
    eList.sort((e1, e2) => {
      if(e1.y != e2.y) {
        return e1.y-e2.y;
      }
      return e1.x-e2.x;
    });

    // Create a new list of sorted elements
    let tList: HTMLElement[] = [];
    for(let i=0; i<eList.length; i++) {
      tList.push(eList[i].ip);
    }

    // Return the sorted element list
    this._sortedInputs=tList;
  }


  /**
   * Function     _GetDocumentEditables
   * Scope        Private
   * @param       {Element}     Pelem     HTML element
   * @return      {Array<Element>}        A list of potentially-editable controls.  Further filtering [as with isKMWInput() and
   *                                      isKMWDisabled()] is required.
   */
  private _GetDocumentEditables(Pelem: HTMLElement): (HTMLElement)[] {
    let possibleInputs: (HTMLElement)[] = [];

    // Document.ownerDocument === null, so we better check that it's not null before proceeding.
    if(Pelem.ownerDocument && Pelem instanceof Pelem.ownerDocument.defaultView.HTMLElement) {
      let dv = Pelem.ownerDocument.defaultView;

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
        return arrayFromNodeList(Pelem.getElementsByTagName(_colon));
      };

      // Note that isKMWInput() will block IFRAME elements as necessary for touch-based devices.
      possibleInputs = possibleInputs.concat(LiTmp('INPUT'), LiTmp('TEXTAREA'), LiTmp('IFRAME'));
    }

    // Not all active browsers may support the method, but only those that do would work with contenteditables anyway.
    if(Pelem.querySelectorAll) {
      possibleInputs = possibleInputs.concat(arrayFromNodeList(Pelem.querySelectorAll('[contenteditable]')));
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
    let possibleInputs = this._GetDocumentEditables(Pelem);

    for(var Li = 0; Li < possibleInputs.length; Li++) {
      // It knows how to handle pre-loaded iframes appropriately.
      this.attachToControl(possibleInputs[Li]);
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
    let possibleInputs = this._GetDocumentEditables(Pelem);

    for(var Li = 0; Li < possibleInputs.length; Li++) {
      // It knows how to handle pre-loaded iframes appropriately.
      this.detachFromControl(possibleInputs[Li]);
    }
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
        window.setTimeout(() => {
          this.listInputs();
        }, 1);
      }
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
  enableControl(Pelem: HTMLElement) {
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

  // To be called by the object responsible for webpage-integration.
  initMutationObservers(document: Document, manualAttach: boolean) {
    if(typeof MutationObserver == 'function') {
      var observationTarget = document.querySelector('body'), observationConfig: MutationObserverInit;
      if(manualAttach) { //I1961
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
    } else {
      console.warn("Your browser is outdated and does not support MutationObservers, a web feature " +
        "needed by KeymanWeb to support dynamically-added elements.");
    }
  }

  shutdown() {
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
    } catch (e) {
      console.error("Error occurred during shutdown");
      console.error(e);
    }
  }
}