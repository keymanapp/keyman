import { EventEmitter } from 'eventemitter3';

import { DeviceSpec, InternalKeyboardFont } from "keyman/engine/keyboard";
import { Input, nestedInstanceOf, wrapElement } from "keyman/engine/element-wrappers";
import {
  arrayFromNodeList,
  createStyleSheet,
  getAbsoluteX,
  getAbsoluteY,
  StylesheetManager
} from "keyman/engine/dom-utils";

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
  /**
   * For any elements being attached or being re-enabled after having been disabled.
   */
  'enabled': (obj: HTMLElement) => void;

  /**
   * For any elements being detached, disabled, or selectively not being attached due
   * being pre-marked for a disabled state.
   */
  'disabled': (obj: HTMLElement) => void;
}

export interface PageAttachmentOptions {
  /**
   * The DeviceSpec metadata for the actual device hosting the active webpage.
   */
  hostDevice: DeviceSpec;

  /**
   * If contained within an iframe, this should be set to .document's owning iframe.
   */
  owner?: HTMLIFrameElement;
}

/*
 * Note:  part of this class's design is to facilitate unit testing for the core
 * attachment algorithm - for validating the logic that determines which elements
 * gain attachment.
 */

/**
 * This class is responsible for Keyman Engine for Web's
 * "attachment" mechanism, which is used to hook into page elements to determine
 * the user's active context and to receive any keystrokes said context receives.
 */
export class PageContextAttachment extends EventEmitter<EventMap> {
  // Note:  we only seem to rely on `device.touchable` within this class?  None of the other properties.
  private readonly options: PageAttachmentOptions;

  public get device(): DeviceSpec {
    return this.options.hostDevice;
  }

  public readonly document: Document;
  protected readonly owner: HTMLIFrameElement;
  private baseFont: string = '';
  private appliedFont: string = '';
  private stylesheetManager: StylesheetManager;

  public get window(): Window {
    return this.document.defaultView;
  }

  private embeddedPageContexts: PageContextAttachment[] = [];

  // Only used for `shutdown`; order doesn't matter.
  private _inputList: HTMLElement[] = [];

  /**
   * Returns a list of all elements attached by this object or one of its children.
   * Note that the list should be considered unordered, as its entries will not
   * be in order of definition within the document.
   */
  public get inputList(): readonly HTMLElement[] {
    let embeddedInputs = this.embeddedPageContexts.map(
      // Gets the input list for any pages embedded via iframe
      (embeddedPage) => embeddedPage.inputList
    ).reduce(
      // Flattens the resulting arrays into a 1D array.
      (flattenedInputList, pageInputList) => flattenedInputList.concat(pageInputList), []
    );

    return [].concat(this._inputList).concat(embeddedInputs);
  }

  // Useful for `moveToNext` operations:  order matters.
  // Note that it only includes `input` and `textarea` elements of the top-level document!
  // Anything in embedded iframes was always ignored for this.
  private _sortedInputs: HTMLElement[] = [];

  /**
   * Returns a list of the Input and Textarea elements hosted by the represented document,
   * in 2D sorted order:  top to bottom, then left to right.
   */
  public get sortedInputs(): ReadonlyArray<HTMLElement> {
    return this._sortedInputs;
  }

  private manualAttach: boolean;

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

  // Fields & properties done; now for the 'meat'.

  /**
   * Prepares the page context-attachment instance for the corresponding document.
   * Does not actually attach until `.attach()` is called.
   * @param document
   * @param options
   */
  constructor(document: Document, options: PageAttachmentOptions) {
    if(!document) {
      throw new Error("Cannot attach to a null/undefined document");
    }

    super();
    this.options = options;
    this.document = document;
    this.stylesheetManager = new StylesheetManager(this.document.body);
  }

  // Note:  `install()` must be separate from construction - otherwise, there's no time
  // interval available to attach event handlers before the attachment process begins.

  /*
   * Call this method **once**, when the page is fully loaded, to attach to all page elements
   * eligible to serve as context for Keyman keyboard input.
   */
  install(manualAttach: boolean) {
    // This field gets referenced by any non-design iframes detected during _SetupDocument.
    // Thus, we must initialize it now.
    this.manualAttach = manualAttach;
    this.baseFont = this.getBaseFont();

    if(!this.manualAttach) {
      this._SetupDocument(this.document.documentElement);

      // Create an ordered list of all input and textarea fields
      this.listInputs();
    }

    // KMW 16.0 and before:  these were only ever established for the top-level doc, and so for
    // 17.0 we'll keep it that way initially.
    //
    // That said, for future consideration:  enable it within iframe-internal documents too.
    if(!this.options.owner) {
      this.initMutationObservers(this.document, manualAttach);
    }
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
      if (Input.isSupportedType(x.type)) {
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
        } else {
          // If the element is being wholesale-deleted, the contentWindow may be gone...
          // but a previously-placed attachment object may yet remain.
          return !!x._kmwAttachment;
        }
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
    if(x._kmwAttachment) {
      return true;
    }

    // A non-design IFrame is 'attached' if there is a corresponding PageContextAttachment instance.
    // ... which could be this one!
    if(nestedInstanceOf(x, 'HTMLIFrameElement')) {
      const iframe = x as HTMLIFrameElement;
      if(iframe.contentDocument == this.document) {
        return true;
      }

      // If not this one, perhaps a child?
      for(let child of this.embeddedPageContexts) {
        if(child.isAttached(x)) {
          return true;
        }
      }
    }

    return false;
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

    // Exists for some HTMLElements, such as HTMLInputElement.
    // @ts-ignore
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
        this._inputList.push(Pelem);

        this.emit('enabled', Pelem);
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
  disableInputElement(Pelem: HTMLElement) {
    if(!Pelem) {
      return;
    }

    // Do NOT test for pre-disabledness - we also use this to fully detach without officially 'disabling' via kmw-disabled.
    if((Pelem.ownerDocument.defaultView && Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) ||
        Pelem instanceof HTMLIFrameElement) {
      this._DetachFromIframe(Pelem);
    } else {
      if(this.isAttached(Pelem)) {
        const intendedInputMode = Pelem._kmwAttachment?.inputMode;

        this.disableInputModeObserver();
        // restores the last-known setting before KMW forced it to 'none'.
        // Refer to enableInputElement.
        Pelem.inputMode = intendedInputMode;
        this.enableInputModeObserver();
      }

      let cnIndex = Pelem.className.indexOf('keymanweb-font');
      if(cnIndex >= 0) { // See note about the alias below.
        Pelem.className = Pelem.className.replace('keymanweb-font', '').trim();
      }

      // Remove the element from our internal input tracking.
      var index = this.inputList.indexOf(Pelem);
      if(index > -1) {
        this._inputList.splice(index, 1);
      }
      this.emit('disabled', Pelem);
    }

    return;
  };

  //#region Prime for removal! ----------------------

  // TODO:  I missed this before, but the 'enable' and 'disable' `TouchElement` method variants should
  // be 100% safe to remove now that touch-aliases are out!
  //
  // I've pre-emptively removed the `touchEnabled` flag, since the handlers are already so pruned as to make
  // said flag unnecessary.

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
      this.emit('disabled', Pelem);
      return false;
    }

    if(!this.isAttached(Pelem)) {
      this.setupElementAttachment(Pelem);
    }

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
  }

  //#endregion Prime for removal! ----------------------

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
      const Lelem=Pelem.contentWindow.document;
      /* editable Iframe */
      if(Lelem) {
        if(Lelem.designMode.toLowerCase() == 'on') {
          // Set up a reference alias; the internal document will need the same attachment info!
          this.setupElementAttachment(Pelem);
          Lelem.body._kmwAttachment = Pelem._kmwAttachment;

          this._inputList.push(Pelem);
          this.emit('enabled', Pelem);
        } else {
          // If already attached, do not attempt to attach again.
          if(this.embeddedPageContexts.filter((context) => context.document == Lelem).length == 0) {
            // Lelem is the IFrame's internal document; set 'er up!
            let embeddedPageAttachment = new PageContextAttachment(Lelem, {
              ...this.options,
              owner: Pelem
            });

            this.embeddedPageContexts.push(embeddedPageAttachment);
            // Forward any attached elements from the embedded page as if we attached them directly.
            embeddedPageAttachment.on('enabled',  (elem) => this.emit('enabled',  elem));
            embeddedPageAttachment.on('disabled', (elem) => this.emit('disabled', elem));

            embeddedPageAttachment.install(this.manualAttach);
          }
        }
      }
    } catch(err) {
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
    const detachFromDesignIframe = () => {
      this.clearElementAttachment(Pelem);

      let index = this._inputList.indexOf(Pelem);
      if(index != -1) {
        this._inputList.splice(index, 1);
      }
      this.emit('disabled', Pelem);
    }

    try {
      const Lelem=Pelem.contentWindow.document;
      /* editable Iframe */
      if(Lelem) {
        if(Lelem.designMode.toLowerCase() == 'on') {
          // Remove the reference to our prior attachment data!
          Lelem.body._kmwAttachment = null; // is an extra step needed for this case.

          detachFromDesignIframe();
        } else {
          // If already attached, do not attempt to attach again.
          for(let i=0; i < this.embeddedPageContexts.length; i++) {
            if(this.embeddedPageContexts[i].document == Lelem) {
              // Pops the entry from the array and maintains a reference to it.
              const embeddedPageAttachment = this.embeddedPageContexts.splice(i, 1)[0];

              embeddedPageAttachment._ClearDocument(Lelem.body); // I2404 - Manage IE events in IFRAMEs
              // The events defined in _AttachToIframe will still forward during `shutdown`.
              embeddedPageAttachment.shutdown();

              // Also, remove child attachment-engine, too.
              this.embeddedPageContexts.splice(i, 1);
              break;
            }
          }
        }
      }
    } catch(err) {
      // If we were previously attached but the content doc/window have been unloaded,
      // we can at least address attachment via the attachment object.
      if(Pelem._kmwAttachment) {
        detachFromDesignIframe();
      }

      // Otherwise, do not attempt to attach to/detach from the iframe;
      // as it's likely from another domain - XSS denied!
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
      if(this.isKMWDisabled(Pelem)) {
        // Future improvement - go ahead and attach, but in disabled state?
        // Can't use `enableControl` later without that... or without the page
        // directly manipulating `kmw-disabled`, anyway.
        // Note:  this idea may require some shuffling of iframe-related handling
        // to handle everything cleanly.
        this.emit('disabled', Pelem);
      } else {
        if(touchable) {
          this.enableTouchElement(Pelem);
        } else {
          this.enableInputElement(Pelem);
        }
      }
    } else if(touchable) {
      // Maybe an 'invalid' instead?
      this.emit('disabled', Pelem);
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
    } else if(nestedInstanceOf(Pelem, "HTMLIFrameElement")) {
      // Future fix idea for this case:  when disabling a normal-iframe, keep the child instance.
      // Just call 'shutdown' on it.  Then, re-'install' here.
      // Current architecture unfortunately conflates 'enable' and 'detach' for iframes, though. :(
      // Should be 'easy enough' to address if and when the time comes.
      // But for now, this'll keep things smoothed over.
      this._AttachToIframe(Pelem as HTMLIFrameElement);
    }
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
    // Current architecture unfortunately conflates 'enable' and 'detach' for iframes, so a
    // disabled iframe appears detached.
    if(!this.isAttached(Pelem) && !nestedInstanceOf(Pelem, "HTMLIFrameElement")) {
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

  // Create an ordered list of all text and search input elements and textarea elements
  // except any tagged with class 'kmw-disabled'
  // TODO: email and url types should perhaps use default keyboard only
  listInputs() {
    let eList: SortableInput[]=[];
    let t1=document.getElementsByTagName('input');
    let t2=document.getElementsByTagName('textarea');

    for(let i=0; i<t1.length; i++) {
      if (Input.isSupportedType(t1[i].type) && t1[i].className.indexOf('kmw-disabled') < 0) {
        eList.push({ip:t1[i], x: getAbsoluteX(t1[i]), y: getAbsoluteY(t1[i])});
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
   * Move focus to next (or previous) input or text area element on TAB
   *   Uses list of actual input elements
   *
   *   Note that activeElement() on touch devices returns the DIV that overlays
   *   the input element, not the element itself.
   *
   * @param      {number|boolean}  bBack     Direction to move (0 or 1)
   */
  findNeighboringInput(activeBase: HTMLElement, bBack: number|boolean) {
    var i,t=this.sortedInputs;

    if(t.length == 0) {
      return null;
    }

    // Identify the active element in the list of inputs ordered by position
    for(i=0; i<t.length; i++) {
      if(t[i] == activeBase) {
        break;
      }
    }

    // If the array is empty or does not hold the element, reverse by one so that
    // either the last (bBack: true) or the first (bBack: false) element is selected.
    if(i == t.length && !bBack) { // otherwise, ... "or the second".
      i--;
    }

    // Find the next (or previous) element in the list
    i = bBack ? i-1 : i+1;
    // Treat the list as circular, wrapping the index if necessary.
    i = i >= t.length ? i-t.length : i;
    i = i < 0 ? i+t.length : i;

    return t[i];
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
  private _SetupDocument(Pelem: HTMLElement) { // I1961
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
  private _ClearDocument(Pelem: HTMLElement) { // I1961
    let possibleInputs = this._GetDocumentEditables(Pelem);

    for(var Li = 0; Li < possibleInputs.length; Li++) {
      // It knows how to handle pre-loaded iframes appropriately.
      this.detachFromControl(possibleInputs[Li]);
    }
  }


  _EnablementMutationObserverCore = (mutations: MutationRecord[]) => {
    for(var i=0; i < mutations.length; i++) {
      var mutation = mutations[i];

      // ( ? : ) needed as a null check.
      var disabledBefore = mutation.oldValue ? mutation.oldValue.indexOf('kmw-disabled') >= 0 : false;
      var disabledAfter = (mutation.target as HTMLElement).className.indexOf('kmw-disabled') >= 0;

      if(disabledBefore && !disabledAfter) {
        this._EnableControl(mutation.target as HTMLElement);
      } else if(!disabledBefore && disabledAfter) {
        this._DisableControl(mutation.target as HTMLElement);
      }

      // 'readonly' triggers on whether or not the attribute exists, not its value.
      if(!disabledAfter && mutation.attributeName == "readonly") {
        var readonlyBefore = mutation.oldValue ? mutation.oldValue != null : false;
        var elem = mutation.target;

        if(elem instanceof elem.ownerDocument.defaultView.HTMLInputElement
            || elem instanceof elem.ownerDocument.defaultView.HTMLTextAreaElement) {
          var readonlyAfter = elem.readOnly;

          if(readonlyBefore && !readonlyAfter) {
            this._EnableControl(mutation.target as HTMLElement);
          } else if(!readonlyBefore && readonlyAfter) {
            this._DisableControl(mutation.target as HTMLElement);
          }
        }
      }
    }
  };

  _AutoAttachObserverCore = (mutations: MutationRecord[]) => {
    var inputElementAdditions: HTMLElement[] = [];
    var inputElementRemovals: HTMLElement[] = [];

    for(var i=0; i < mutations.length; i++) {
      let mutation = mutations[i];

      for(var j=0; j < mutation.addedNodes.length; j++) {
        inputElementAdditions = inputElementAdditions.concat(this._GetDocumentEditables(mutation.addedNodes[j] as HTMLElement));
      }

      for(j = 0; j < mutation.removedNodes.length; j++) {
        inputElementRemovals = inputElementRemovals.concat(this._GetDocumentEditables(mutation.removedNodes[j] as HTMLElement));
      }
    }

    for(var k = 0; k < inputElementAdditions.length; k++) {
      if(this.isKMWInput(inputElementAdditions[k])) { // Apply standard element filtering!
        this._MutationAdditionObserved(inputElementAdditions[k]);
      }
    }

    for(k = 0; k < inputElementRemovals.length; k++) {
      let matched = false;
      const elem = inputElementRemovals[k];

      // Note:  for iframes, .contentWindow has already been deleted by this point!
      if(elem instanceof elem.ownerDocument.defaultView.HTMLIFrameElement) {
        // Non-design iframes
        for(let i = 0; i < this.embeddedPageContexts.length; i++) {
          if(this.embeddedPageContexts[i].options.owner == elem) {
            // we can't do the standard detachment anymore, since the document and its elements
            // have been obliterated.  BUT!  We still have our .inputList and can handle
            // things that way!
            this.embeddedPageContexts[i].shutdown();

            // Also, remove the child attachment-engine, too.
            this.embeddedPageContexts.splice(i, 1);
            matched = true;
            break;
          }
        }

        if(!matched) {
          // Time to check for design-mode iframe attachment...
          for(let i = 0; i < this._inputList.length; i++) {
            if(this._inputList[i] == elem) {
              // We have a match!  Fortunately, this case is simpler from here.
              this.detachFromControl(elem);

              // Since the iframe itself was removed from the hierarchy, the detachment
              // design-mode check will likely fail.  Double-check for robustness, but
              // we'll need to tidy up our _inputList here.
              if(this._inputList[i] == elem) {
                this._inputList.splice(i, 1);
              }
              break;
            }
          }
        }
      } else if(this.isKMWInput(elem)) {
        this._MutationRemovalObserved(elem);
      }
    }

    /* After all mutations have been handled, we need to recompile our .sortedInputs array, but only
      * if any have actually occurred.
      */
    if(inputElementAdditions.length || inputElementRemovals.length) {
      if(!this.device.touchable) {
        this.listInputs();
      } else if(this.device.touchable) {   // If something was added or removed, chances are it's gonna mess up our touch-based layout scheme, so let's update the touch elements.
        window.setTimeout(() => {
          this.listInputs();
        }, 1);
      }
    }
  };

  /**
   * Function     _MutationAdditionObserved
   * Scope        Private
   * @param       {Element}  Pelem     A page input, textarea, or iframe element.
   * Description  Used by the MutationObserver event handler to properly setup any elements dynamically added to the document post-initialization.
   *
   */
  _MutationAdditionObserved = (Pelem: HTMLElement) => {
    if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
      //Problem:  the iframe is loaded asynchronously, and we must wait for it to load fully before hooking in.

      const attachFunctor = () => {  // Triggers at the same time as iframe's onload property, after its internal document loads.
        // Provide a minor delay to allow 'load' event handlers to set the design-mode property.
        window.setTimeout(() => {
          this.attachToControl(Pelem);
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
  _MutationRemovalObserved = (Pelem: HTMLElement) => {
    this.detachFromControl(Pelem); // Remove all KMW event hooks, styling.
  }

  // To be called by the object responsible for webpage-integration.
  initMutationObservers(document: Document, manualAttach: boolean) {
    if(typeof MutationObserver == 'function') {
      var observationTarget = document.querySelector('body'), observationConfig: MutationObserverInit;
      if(!manualAttach) { //I1961
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

      this.inputModeObserver = new MutationObserver(this._InputModeObserverCore);
      this.enableInputModeObserver();
    } else {
      console.warn("Your browser is outdated and does not support MutationObservers, a web feature " +
        "needed by KeymanWeb to support dynamically-added elements.");
    }
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
   * Get the user-specified (or default) font for the first mapped input or textarea element
   * before applying any keymanweb styles or classes
   *
   *  @return   {string}
   */
  getBaseFont() {
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
        break;
      case 1:
        fs = getComputedStyle(ipInput[0]).fontFamily || '';
        break;
      case 2:
        fs = getComputedStyle(ipTextArea[0]).fontFamily || '';
        break;
    }
    if(typeof(fs) == 'undefined' || fs == 'monospace') {
      fs=fsDefault;
    }

    return fs;
  }

  /**
   *  Add or replace the style sheet used to set the font for input elements
   *
   *  @param  {Object}  kfd   KFont font descriptor
   *  @return {string}
   *
   **/
  buildAttachmentFontStyle(keyboardFontDescriptor: InternalKeyboardFont): string {
    let kfd = keyboardFontDescriptor;

    // Get name of font to be applied
    let fontName = this.baseFont;
    if (kfd && typeof (kfd.family) != 'undefined') {
      fontName = kfd['family']; // If we have a font set by the keyboard, prioritize that over the base font.
    }

    // Unquote font name in base font (if quoted)
    fontName = fontName.replace(/\u0022/g, '');

    // Set font family chain for mapped elements and remove any double quotes
    // font-family:  maintains the base font as a fallback.
    var rx = new RegExp('\\s?' + fontName + ',?'), fontFamily = this.appliedFont.replace(/\u0022/g, '');

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
    let s = '.keymanweb-font{\nfont-family:' + fontFamily + ' !important;\n}\n';

    // Store the current font chain (with quote-delimited font names)
    this.appliedFont = fontFamily;

    // Return the style string
    return s;
  }

  setAttachmentFont(
    keyboardFontDescriptor: InternalKeyboardFont,
    fontRoot: string,
    os: DeviceSpec.OperatingSystem
  ) {
    this.stylesheetManager.unlinkAll();
    this.stylesheetManager.addStyleSheetForFont(keyboardFontDescriptor, fontRoot, os);
    this.stylesheetManager.linkStylesheet(createStyleSheet(this.buildAttachmentFontStyle(keyboardFontDescriptor)));

    // Future note:  might be worth propagating to any child documents (embedded iframes) via
    // our child instances of this class. (via `this.embeddedPageContexts`)
  }

  shutdown() {
    try {
      this.enablementObserver?.disconnect();
      this.attachmentObserver?.disconnect();
      this.inputModeObserver?.disconnect();
      this.stylesheetManager?.unlinkAll();

      /*
       * Part of shutdown involves detaching from elements... which typically does involve
       * restoring the original `inputMode` settings.  Remove the observer reference after
       * disconnection to prevent any further disable-enable actions.
       *
       * The others aren't toggled, so they're fine to leave.
       */
      this.inputModeObserver = null;

      // Embedded pages first - that way, each page can handle its own inputs, rather than having
      // the top-level instance handle all attached elements.
      // (inputList enumerates child pages, too!)
      this.embeddedPageContexts.forEach((embeddedPage) => {
        try {
          embeddedPage.shutdown();
        } catch (e) {}
      });

      for(let input of this.inputList) {
        try {
          this.detachFromControl(input);
        } catch(e) {
          // If the element was within an iframe's document AND the iframe itself was
          // removed, we can no longer read the element's properties.
          // `.isKMWInput()` may throw an error then.
          this.emit('disabled', input);
        }
      }
      this._inputList = [];
    } catch (e) {
      console.error("Error occurred during shutdown");
      console.error(e);
    }
  }
}
