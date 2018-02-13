// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// References the base KMW object.
/// <reference path="kmwbase.ts" />
// References DOM event handling interfaces and classes.
/// <reference path="kmwdomevents.ts" />
// Includes KMW string extension declarations.
/// <reference path="kmwstring.ts" />

/**
 * This class serves as the intermediary between KeymanWeb and any given web page's elements.
 */
class DOMManager {
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
   * Tracks a list of event-listening elements.  
   * 
   * In touch mode, this should contain touch-aliasing DIVs, but will contain other elements in non-touch mode.
   */
  inputList: HTMLElement[] = [];            // List of simulated input divisions for touch-devices   I3363 (Build 301)

  /**
   * Tracks a visually-sorted list of elements that are KMW-enabled.
   */
  sortedInputs: HTMLElement[] = [];   // List of all INPUT and TEXTAREA elements ordered top to bottom, left to right

  constructor(keyman: KeymanBase) {
    this.keyman = keyman;
    
    if(keyman.util.device.touchable) {
      this.touchHandlers = new DOMTouchHandlers(keyman);
    }

    this.nonTouchHandlers = new DOMEventHandlers(keyman);
  }

  shutdown() {
    if(this.enablementObserver) {
      this.enablementObserver.disconnect();
    }
    if(this.attachmentObserver.disconnect) {
      this.attachmentObserver.disconnect();
    }
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
    // Touch doesn't worry about iframes.
    if(Pelem.tagName.toLowerCase() == 'iframe') {
      return false;
    }

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

    /*
    *  Does this element already have a simulated touch element established?  If so,
    *  just reuse it - if it isn't still in the input list!
    */
    if(Pelem['kmw_ip']) {

      if(this.inputList.indexOf(Pelem['kmw_ip']) != -1) {
        return false;
      }

      this.inputList.push(Pelem['kmw_ip']);
      
      console.log("Unexpected state - this element's simulated input DIV should have been removed from the page!");

      return true;   // May need setup elsewhere since it's just been re-added!
    }

    // The simulated touch element doesn't already exist?  Time to initialize it.
    var x=document.createElement<'div'>('div'); 
    x['base']=x.base=Pelem;
    x._kmwAttachment = Pelem._kmwAttachment; // It's an object reference we need to alias.
    
    // Set font for base element
    this.enableInputElement(x, true);

    // Add the exposed member 'kmw_ip' to allow page to refer to duplicated element
    Pelem['kmw_ip']=x;
    Pelem.disabled = true;

    // Superimpose custom input fields for each input or textarea, unless readonly or disabled 

    // Copy essential styles from each base element to the new DIV      
    var d,bs,xs,ds,ss1,ss2,ss3,x1,y1;

    x.className='keymanweb-input';
    x.dir=x.base.dir;
    
    // Add a scrollable interior div 
    d=document.createElement<'div'>('div'); 
    bs=window.getComputedStyle(x.base,null);
    xs=x.style;
    xs.overflow='hidden';
    xs.position='absolute';
    //xs.border='1px solid gray';
    xs.border='hidden';      // hide when element empty - KMW-3
    xs.border='none';
    xs.borderRadius='5px';

    // Add a scroll bar (horizontal for INPUT elements, vertical for TEXTAREA elements)
    var sb=document.createElement<'div'>('div'), sbs=sb.style;
    sbs.position='absolute';
    sbs.height=sbs.width='4px';
    sbs.left=sbs.top='0';
    sbs.display='block';
    sbs.visibility='hidden';          
    sbs.backgroundColor='#808080';
    sbs.borderRadius='2px';
    
    var s1: HTMLSpanElement, s2: HTMLSpanElement, s3: HTMLSpanElement;

    // And add two spans for the text content before and after the caret, and a caret span
    s1=document.createElement<'span'>('span');
    s2=document.createElement<'span'>('span');
    s3=document.createElement<'span'>('span');      
    s1.innerHTML=s2.innerHTML=s3.innerHTML='';
    s1.className=s2.className=s3.className='keymanweb-font';
    d.appendChild(s1);
    d.appendChild(s3);
    d.appendChild(s2);
    x.appendChild(d);
    x.appendChild(sb);

    // Adjust input element properties so that it matches the base element as closely as possible
    ds=d.style;
    ds.position='absolute'; 

    ss1=s1.style;ss2=s2.style;ss3=s3.style;ss1.border=ss2.border='none';
    //ss1.backgroundColor='rgb(220,220,255)';ss2.backgroundColor='rgb(220,255,220)'; //only for testing 
    ss1.height=ss2.height='100%';          
    ss1.fontFamily=ss2.fontFamily=ds.fontFamily=bs.fontFamily;

    // Set vertical centering for input elements
    if(x.base.nodeName.toLowerCase() == 'input') {
      if(!isNaN(parseInt(bs.height,10))) {
        ss1.lineHeight=ss2.lineHeight=bs.height;      
      }
    }
    
    // The invisible caret-positioning span must have a border to ensure that 
    // it remains in the layout, but colour doesn't matter, as it is never visible.
    // Span margins are adjusted to compensate for the border and maintain text positioning.  
    ss3.border='1px solid red';  
    ss3.visibility='hidden';       
    ss3.marginLeft=ss3.marginRight='-1px';
    
    // Set the outer element padding *after* appending the element, 
    // otherwise Firefox misaligns the two elements
    xs.padding='8px';
    
    // Set internal padding to match the TEXTAREA and INPUT elements
    ds.padding='0px 2px'; // OK for iPad, possibly device-dependent

    if(this.keyman.util.device.OS == 'Android' && bs.backgroundColor == 'transparent') {
      ds.backgroundColor='#fff';
    } else {
      ds.backgroundColor=bs.backgroundColor;
    }
    
    // Set the tabindex to 0 to allow a DIV to accept focus and keyboard input 
    // c.f. http://www.w3.org/WAI/GL/WCAG20/WD-WCAG20-TECHS/SCR29.html
    x.tabIndex=0; 

    // Disable (internal) pan and zoom on KMW input elements for IE10
    x.style.msTouchAction='none';

    // On touch event, reposition the text caret and prepare for OSK input
    // Removed 'onfocus=' as that resulted in handling the event twice (on iOS, anyway) 

    // We know this to be the correct set of handlers because we're setting up a touch element.
    var touchHandlers = this.touchHandlers;
    
    x.addEventListener('touchstart', touchHandlers.setFocus);
    x.onmspointerdown=function(e) {
      e.preventDefault();
      e.stopPropagation();
      return touchHandlers.setFocus(e);
    };

    x.addEventListener('touchend', function(e) {
      e.stopPropagation();
    });

    x.onmspointerup=function(e) {
      e.stopPropagation();
    };
    
    // Disable internal scroll when input element in focus 
    x.addEventListener('touchmove', touchHandlers.dragInput, false);
    x.onmspointermove=touchHandlers.dragInput;
    
    // Hide keyboard and caret when losing focus from simulated input field
    x.onblur=touchHandlers.setBlur;
    
    // Note that touchend event propagates and is processed by body touchend handler
    // re-setting the first touch point for a drag

    if(x.base.nodeName.toLowerCase() == 'textarea') {
      s1.style.whiteSpace=s2.style.whiteSpace='pre-wrap'; //scroll vertically
    } else {
      s1.style.whiteSpace=s2.style.whiteSpace='pre';      //scroll horizontally
    }
    
    x.base.parentNode.appendChild(x);
  
    // Refresh style pointers, and match the field sizes
    touchHandlers.updateInput(x);
    xs=x.style; 
    xs.color=bs.color; //xs.backgroundColor=bs.backgroundColor; 
    xs.fontFamily=bs.fontFamily; xs.fontSize=bs.fontSize;
    xs.fontWeight=bs.fontWeight; xs.textDecoration=bs.textDecoration;
    xs.padding=bs.padding; xs.margin=bs.margin; 
    xs.border=bs.border; xs.borderRadius=bs.borderRadius;
  
    //xs.color='red';  //use only for checking alignment

    // Prevent highlighting of underlying element (Android)
    if('webkitTapHighlightColor' in xs) {
      xs.webkitTapHighlightColor='rgba(0,0,0,0)';
    }

    if(x.base instanceof x.base.ownerDocument.defaultView.HTMLTextAreaElement) {
      // Correct rows value if defaulted and box height set by CSS
      // The rows value is used when setting the caret vertically

      if(x.base.rows == 2) { // 2 is default value
        var h=parseInt(bs.height,10)-parseInt(bs.paddingTop,10)-parseInt(bs.paddingBottom,10),
          dh=parseInt(bs.fontSize,10),calcRows=Math.round(h/dh);
        if(calcRows > x.base.rows+1) {
          x.base.rows=calcRows;
        }
      }
      ds.width=xs.width; ds.minHeight=xs.height;
    } else {
      ds.minWidth=xs.width; ds.height=xs.height;
    }
    x.base.style.visibility='hidden'; // hide by default: KMW-3
    
    // Add an explicit event listener to allow the duplicated input element 
    // to be adjusted for any changes in base element location or size
    // This will be called for each element after any rotation, as well as after user-initiated changes
    // It has to be wrapped in an anonymous function to preserve scope and be applied to each element.
    (function(xx){
      xx._kmwResizeHandler = function(e){
        /* A timeout is needed to let the base element complete its resizing before our 
        * simulated element can properly resize itself.
        * 
        * Not doing this causes errors if the input elements are resized for whatever reason, such as
        * changing languages to a text with greater height.
        */
        window.setTimeout(function (){
          touchHandlers.updateInput(xx);
        }, 1);
      };

      xx.base.addEventListener('resize', xx._kmwResizeHandler, false);
    })(x);

    var textValue: string;

    if(x.base instanceof x.base.ownerDocument.defaultView.HTMLTextAreaElement 
        || x.base instanceof x.base.ownerDocument.defaultView.HTMLInputElement) {
      textValue = x.base.value;
    } else {
      textValue = x.base.textContent;
    }
      
    // And copy the text content
    touchHandlers.setText(x, textValue, null);  

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

    // Touch doesn't worry about iframes.
    if(Pelem.tagName.toLowerCase() == 'iframe') {
      return; // If/when we do support this, we'll need an iframe-level manager for it.
    }

    if(Pelem['kmw_ip']) {
      var index = this.inputList.indexOf(Pelem['kmw_ip']);
      if(index != -1) {
        this.inputList.splice(index, 1);
      }

      Pelem.style.visibility='visible'; // hide by default: KMW-3
      Pelem.disabled = false;
      Pelem.removeEventListener('resize', Pelem['kmw_ip']._kmwResizeHandler);

      // Disable touch-related handling code.
      this.disableInputElement(Pelem['kmw_ip']);
      
      // We get weird repositioning errors if we don't remove our simulated input element - and permanently.
      if(Pelem.parentNode) {
        Pelem.parentNode.removeChild(Pelem['kmw_ip']);
      }
      delete Pelem['kmw_ip'];
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
    x.addEventListener('touchstart', this.nonKMWTouchHandler, false);

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
  enableInputElement(Pelem: HTMLElement, isAlias?: boolean) { 
    var baseElement = isAlias ? Pelem['base'] : Pelem;
    if(!this.isKMWDisabled(baseElement)) {
      if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
        this._AttachToIframe(Pelem);
      } else { 
        baseElement.className = baseElement.className ? baseElement.className + ' keymanweb-font' : 'keymanweb-font';
        this.inputList.push(Pelem);

        this.keyman.util.attachDOMEvent(baseElement,'focus', this.getHandlers(Pelem)._ControlFocus);
        this.keyman.util.attachDOMEvent(baseElement,'blur', this.getHandlers(Pelem)._ControlBlur);

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
    var baseElement = isAlias ? Pelem['base'] : Pelem;
    // Do NOT test for pre-disabledness - we also use this to fully detach without officially 'disabling' via kmw-disabled.
    if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
      this._DetachFromIframe(Pelem);
    } else { 
      var cnIndex = baseElement.className.indexOf('keymanweb-font');
      if(cnIndex > 0 && !isAlias) { // See note about the alias below.
        baseElement.className = baseElement.className.replace('keymanweb-font', '').trim();
      }

      // Remove the element from our internal input tracking.
      var index = this.inputList.indexOf(Pelem);
      if(index > -1) {
        this.inputList.splice(index, 1);
      }

      if(!isAlias) { // See note about the alias below.
        this.keyman.util.detachDOMEvent(baseElement,'focus', this.getHandlers(Pelem)._ControlFocus);
        this.keyman.util.detachDOMEvent(baseElement,'blur', this.getHandlers(Pelem)._ControlBlur);
      }
      // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
      Pelem.onkeypress = null;
      Pelem.onkeydown = null;
      Pelem.onkeyup = null;      
    }

    // If we're disabling an alias, we should fully enable the base version.  (Thinking ahead to toggleable-touch mode.)
    if(isAlias) {
      this.inputList.push(baseElement);

      baseElement.onkeypress = this.getHandlers(Pelem)._KeyPress;
      baseElement.onkeydown = this.getHandlers(Pelem)._KeyDown;
      baseElement.onkeyup = this.getHandlers(Pelem)._KeyUp;  
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
      this.setupElementAttachment(Pelem);
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
    if(!this.isAttached(Pelem)) {
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
    var touchable = this.keyman.util.device.touchable;

    if(x instanceof x.ownerDocument.defaultView.HTMLTextAreaElement) {
      return true;
    } else if(x instanceof x.ownerDocument.defaultView.HTMLInputElement) {
      if (x.type == 'text' || x.type == 'search') {
        return true;
      }
    } else if(x instanceof x.ownerDocument.defaultView.HTMLIFrameElement && !touchable) { // Do not allow iframe attachment if in 'touch' mode.
      try {
        if(x.contentWindow.document) {  // Only allow attachment if the iframe's internal document is valid.
          return true;
        }
      }
      catch(err) { /* Do not attempt to access iframes outside this site */ }
    } else if(x.isContentEditable && !touchable) { // Only allow contentEditable attachment outside of 'touch' mode.
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
      x._kmwAttachment = new AttachmentInfo(null, this.keyman.util.device.touchable);
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
          util.attachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
          util.attachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
          util.attachDOMEvent(Lelem,'keydown', this.getHandlers(Pelem)._KeyDown);
          util.attachDOMEvent(Lelem,'keypress', this.getHandlers(Pelem)._KeyPress);
          util.attachDOMEvent(Lelem,'keyup', this.getHandlers(Pelem)._KeyUp);
        } else {
          // Lelem is the IFrame's internal document; set 'er up!
          this._SetupDocument(Lelem);	   // I2404 - Manage IE events in IFRAMEs
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
          util.detachDOMEvent(Lelem,'focus', this.getHandlers(Pelem)._ControlFocus);
          util.detachDOMEvent(Lelem,'blur', this.getHandlers(Pelem)._ControlBlur);
          util.detachDOMEvent(Lelem,'keydown', this.getHandlers(Pelem)._KeyDown);
          util.detachDOMEvent(Lelem,'keypress', this.getHandlers(Pelem)._KeyPress);
          util.detachDOMEvent(Lelem,'keyup', this.getHandlers(Pelem)._KeyUp);
        } else {
          // Lelem is the IFrame's internal document; set 'er up!
          this._ClearDocument(Lelem);	   // I2404 - Manage IE events in IFRAMEs
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
  _GetDocumentEditables(Pelem: HTMLElement|Document): (HTMLElement|Document)[] {
    var util = this.keyman.util;

    var possibleInputs: (HTMLElement|Document)[] = [];

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
  _SetupDocument(Pelem: HTMLElement|Document) { // I1961
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
  _ClearDocument(Pelem: HTMLElement|Document) { // I1961
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
    var elDir=((this.keyman.keyboardManager.activeKeyboard != null) && (this.keyman.keyboardManager.activeKeyboard['KRTL'])) ? 'rtl' : 'ltr';

    if(Ptarg) {
      if(this.keyman.util.device.touchable) {
        if(Ptarg.textContent.length == 0) {
          Ptarg.base.dir=Ptarg.dir=elDir;
          this.getHandlers(Ptarg).setTextCaret(Ptarg,10000);
        }
      } else {
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
  }

  /**
   * Function     _DisableControl
   * Scope        Private
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disable KMW control element 
   */    
  _DisableControl(Pelem: HTMLElement) {
    if(this.isAttached(Pelem)) { // Only operate on attached elements!
      var lastElem = this.getLastActiveElement();
      if(lastElem == Pelem || lastElem == Pelem['kmw_ip']) {
        this.clearLastActiveElement();
        this.keyman.keyboardManager.setActiveKeyboard(this.keyman.globalKeyboard, this.keyman.globalLanguageCode);
        this.keyman.osk._Hide();
      }
      
      if(this.keyman.util.device.touchable) {
        this.disableTouchElement(Pelem);
        this.setupNonKMWTouchElement(Pelem);

        var keyman = this.keyman;
      
        // If a touch alias was removed, chances are it's gonna mess up our touch-based layout scheme, so let's update the touch elements.
        window.setTimeout(function() {
          this.listInputs();

          for(var k = 0; k < this.sortedInputs.length; k++) {
            if(DOMEventHandlers.states[k]['kmw_ip']) {
              this.getHandlers(Pelem).updateInput(DOMEventHandlers.states[k]['kmw_ip']);
            }
          }
        }.bind(this), 1);
      } else {
        this.listInputs(); // Fix up our internal input ordering scheme.
      }
      
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

          for(var k = 0; k < this.sortedInputs.length; k++) {
            if(this.sortedInputs[k]['kmw_ip']) {
              this.getHandlers(Pelem).updateInput(this.sortedInputs[k]['kmw_ip']);
            }
          }
        }, 1);
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
            eList.push({ip:t1[i],x:util._GetAbsoluteX(t1[i]),y:util._GetAbsoluteY(t1[i])});
          }
          break;    
      }
    }

    for(i=0; i<t2.length; i++) { 
      if(t2[i].className.indexOf('kmw-disabled') < 0)
        eList.push({ip:t2[i],x:util._GetAbsoluteX(t2[i]),y:util._GetAbsoluteY(t2[i])});
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

          for(var k = 0; k < this.sortedInputs.length; k++) {
            if(this.sortedInputs[k]['kmw_ip']) {
              this.keyman.touchAliasing.updateInput(this.sortedInputs[k]['kmw_ip']);
            }
          }
        }.bind(this), 1);
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
    if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement && !this.keyman.util.device.touchable) {
      //Problem:  the iframe is loaded asynchronously, and we must wait for it to load fully before hooking in.

      var domManager = this;

      var attachFunctor = function() {  // Triggers at the same time as iframe's onload property, after its internal document loads.
        domManager.attachToControl(Pelem);
      };

      Pelem.addEventListener('load', attachFunctor);

      /* If the iframe has somehow already loaded, we can't expect the onload event to be raised.  We ought just
      * go ahead and perform our callback's contents.
      * 
      * keymanweb.domManager.attachToControl() is now idempotent, so even if our call 'whiffs', it won't cause long-lasting
      * problems.
      */
      if(Pelem.contentDocument.readyState == 'complete') {
        window.setTimeout(attachFunctor, 1);
      }
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
  _WindowUnload: () => void = function() {
    // Allow the UI to release its own resources
    this.keyman.uiManager.doUnload();
    
    // Allow the OSK to release its own resources
    if(this.keyman.osk.ready) {
      this.keyman.osk._Unload(); // I3363 (Build 301)
    }
    
    this.clearLastActiveElement();
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
      // 'kmw_ip' is the touch-alias for the original page's control.

      var lastElem = this.getLastActiveElement();
      if(lastElem && (lastElem == Pelem || lastElem == Pelem['kmw_ip'])) {

        if(Pkbd != null && Plc != null) { // Second part necessary for Closure.
          this.keyman.keyboardManager.setActiveKeyboard(Pkbd, Plc);
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
    var lastElem = this.getLastActiveElement();
    if(!lastElem) {
      return;
    }

    this.keyman.uiManager.justActivated = true;
    if(lastElem.ownerDocument && lastElem instanceof lastElem.ownerDocument.defaultView.HTMLIFrameElement && 
        this.keyman.domManager._IsMozillaEditableIframe(lastElem as HTMLIFrameElement,0)) {
      lastElem.ownerDocument.defaultView.focus(); // I3363 (Build 301)
    } else if(lastElem.focus) {
      lastElem.focus();
    }
  }

  /**
   * Get the last active target element *before* KMW activated (I1297)
   * 
   * @return      {Element}        
   */    
  getLastActiveElement(): HTMLElement {
    return DOMEventHandlers.states.lastActiveElement;
  }

  clearLastActiveElement() {
    DOMEventHandlers.states.lastActiveElement = null;
  }

  getActiveElement(): HTMLElement {
    return DOMEventHandlers.states.activeElement;
  }

  _setActiveElement(Pelem: HTMLElement) {
    DOMEventHandlers.states.activeElement = Pelem;
  }

  /**
   *  Set the active input element directly optionally setting focus 
   * 
   *  @param  {Object|string} e         element id or element
   *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123) 
   **/
  setActiveElement(e: string|HTMLElement, setFocus?: boolean) {
    if(typeof(e) == "string") { // Can't instanceof string, and String is a different type.
      e=document.getElementById(e);
    }

    DOMEventHandlers.states.activeElement = DOMEventHandlers.states.lastActiveElement=e;

    // Allow external focusing KMEW-123
    if(arguments.length > 1 && setFocus) {
      if(this.keyman.util.device.touchable) {
        this.keyman.touchAliasing.setFocus();
      } else {
        this.focusLastActiveElement();
      }
    }
  }

  /** Sets the active input element only if it is presently null.
   * 
   * @param  {Element}
   */
  initActiveElement(Lelem: HTMLElement) {
    if(DOMEventHandlers.states.activeElement == null) {
      DOMEventHandlers.states.activeElement = Lelem;
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
    var i,t=this.sortedInputs, activeBase=this.getActiveElement();
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
    if(touchable) {
      // Set focusing flag to prevent OSK disappearing 
      DOMEventHandlers.states.focusing=true;
      var target=t[i]['kmw_ip'];

      // Focus if next element is non-mapped
      if(typeof(target) == 'undefined') {
        t[i].focus();
      } else { // Or reposition the caret on the input DIV if mapped
        this.keyman.domManager.setActiveElement(target); // Handles both `lastActive` + `active`.
        this.touchHandlers.setTextCaret(target,10000); // Safe b/c touchable == true.
        this.touchHandlers.scrollInput(target);   // mousedown check
        target.focus();
      }
    } else { // Behaviour for desktop browsers
      t[i].focus();
    }
  }

  /**
   * Move focus to user-specified element
   * 
   *  @param  {string|Object}   e   element or element id
   *           
   **/
  moveToElement(e:string|HTMLElement) {
    var i;
    
    if(typeof(e) == "string") { // Can't instanceof string, and String is a different type.
      e=document.getElementById(e);
    }
    
    if(this.keyman.util.device.touchable && e['kmw_ip']) {
      e['kmw_ip'].focus();
    } else {
      e.focus();
    }
  }

  /* ----------------------- Editable IFrame methods ------------------- */

  /**
   * Function     _IsIEEditableIframe
   * Scope        Private
   * @param       {Object}          Pelem         Iframe element
   *              {boolean|number}  PtestOn       1 to test if frame content is editable (TODO: unclear exactly what this is doing!)   
   * @return      {boolean}
   * Description  Test if element is an IE editable IFrame 
   */    
  _IsIEEditableIframe(Pelem: HTMLIFrameElement, PtestOn?: number) {
    var Ldv, Lvalid = Pelem  &&  (Ldv=Pelem.tagName)  &&  Ldv.toLowerCase() == 'body'  &&  (Ldv=Pelem.ownerDocument)  &&  Ldv.parentWindow;
    return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Pelem.isContentEditable));
  }

  /**
   * Function     _IsMozillaEditableIframe
   * Scope        Private
   * @param       {Object}           Pelem    Iframe element
   * @param       {boolean|number}   PtestOn  1 to test if 'designMode' is 'ON'    
   * @return      {boolean} 
   * Description  Test if element is a Mozilla editable IFrame 
   */    
  _IsMozillaEditableIframe(Pelem: HTMLIFrameElement, PtestOn?: number) {
    var Ldv, Lvalid = Pelem  &&  (Ldv=(<any>Pelem).defaultView)  &&  Ldv.frameElement;  // Probable bug!
    return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Ldv.document.designMode.toLowerCase()=='on'));
  }

  /* ----------------------- Initialization methods ------------------ */
  
  /**
   * Get the user-specified (or default) font for the first mapped input or textarea element
   * before applying any keymanweb styles or classes
   * 
   *  @return   {string}
   **/                 
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
   * @param       {Object}  arg     object array of user-defined properties
   * Description  KMW window initialization  
   */    
  init(arg) { 
    var i,j,c,e,p,eTextArea,eInput,opt,dTrailer,ds;
    var osk = this.keyman.osk;
    var util = this.keyman.util;
    var device = util.device;

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
    }            
    
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

    // Set element attachment type    
    if(opt['attachType'] == '') {
      opt['attachType'] = 'auto';
    }

    // Set default device options
    this.keyman.setDefaultDeviceOptions(opt);   
    
    // Only do remainder of initialization once!  
    if(this.keyman.initialized) {
      return;
    }

    var keyman: KeymanBase = this.keyman;
    var domManager = this;

    // Do not initialize until the document has been fully loaded
    if(document.readyState !== 'complete')
    {
      window.setTimeout(function(){
        domManager.init(arg);
      }, 50);
      return;
    }

    this.keyman._MasterDocument = window.document;

    /**
     * Initialization of touch devices and browser interfaces must be done 
     * after all resources are loaded, during final stage of initialization
     *        
     */            
    
    // Treat Android devices as phones if either (reported) screen dimension is less than 4" 
    if(device.OS == 'Android')
    {
      // Determine actual device characteristics  I3363 (Build 301)
      // TODO: device.dpi may no longer be needed - if so, get rid of it.
      var dpi = device.getDPI(); //TODO: this will not work when called from HEAD!!
      device.formFactor=((screen.height < 4.0 * dpi) || (screen.width < 4.0 * dpi)) ? 'phone' : 'tablet';
    }
  
    // Set exposed initialization flag member for UI (and other) code to use 
    this.keyman.setInitialized(1);

    // Finish keymanweb and OSK initialization once all necessary resources are available
    osk.prepare();
  
    // Create and save the remote keyboard loading delay indicator
    util.prepareWait();

    // Register deferred keyboard stubs (addKeyboards() format)
    this.keyman.keyboardManager.registerDeferredStubs();
  
    // Initialize the desktop UI
    this.initializeUI();
  
    // Register deferred keyboards 
    this.keyman.keyboardManager.registerDeferredKeyboards();
  
    // Exit initialization here if we're using an embedded code path.
    if(this.keyman.isEmbedded) {
      if(!this.keyman.keyboardManager.setDefaultKeyboard()) {
        console.error("No keyboard stubs exist - cannot initialize keyboard!");
      }
      return;
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
      
      // On Chrome, scrolling up or down causes the URL bar to be shown or hidden 
      // according to whether or not the document is at the top of the screen.
      // But when doing that, each OSK row top and height gets modified by Chrome
      // looking very ugly.  Itwould be best to hide the OSK then show it again 
      // when the user scroll finishes, but Chrome has no way to reliably report
      // the touch end event after a move. c.f. http://code.google.com/p/chromium/issues/detail?id=152913
      // The best compromise behaviour is simply to hide the OSK whenever any 
      // non-input and non-OSK element is touched.
      if(device.OS == 'Android' && navigator.userAgent.indexOf('Chrome') > 0) {
        (<any>this.keyman).hideOskWhileScrolling=function(e) {           
          if(typeof(osk._Box) == 'undefined') return;
          if(typeof(osk._Box.style) == 'undefined') return;

          // The following tests are needed to prevent the OSK from being hidden during normal input!
          p=e.target.parentNode;
          if(typeof(p) != 'undefined' && p != null) {
            if(p.className.indexOf('keymanweb-input') >= 0) return; 
            if(p.className.indexOf('kmw-key-') >= 0) return; 
            if(typeof(p.parentNode) != 'undefined') {
              p=p.parentNode;
              if(p.className.indexOf('keymanweb-input') >= 0) return; 
              if(p.className.indexOf('kmw-key-') >= 0) return; 
            }
          }          
          osk.hideNow(); 
        }        
        document.body.addEventListener('touchstart', (<any>this.keyman).hideOskWhileScrolling, false);
      } else {
        (<any>this.keyman).conditionallyHideOsk = function() {
          // Should not hide OSK if simply closing the language menu (30/4/15)
          if((<any>keyman).hideOnRelease && !osk.lgList) osk.hideNow();
          (<any>keyman).hideOnRelease=false;
        };
        (<any>this.keyman).hideOskIfOnBody = function(e) {
          (<any>keyman).touchY=e.touches[0].screenY;
          (<any>keyman).hideOnRelease=true;
        };
        (<any>this.keyman).cancelHideIfScrolling = function(e) {
          var y=e.touches[0].screenY,y0=(<any>keyman).touchY;    
          if(y-y0 > 5 || y0-y < 5) (<any>keyman).hideOnRelease = false;
        };

        document.body.addEventListener('touchstart',(<any>this.keyman).hideOskIfOnBody,false);      
        document.body.addEventListener('touchmove',(<any>this.keyman).cancelHideIfScrolling,false);      
        document.body.addEventListener('touchend',(<any>this.keyman).conditionallyHideOsk,false);      
      } 
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
    
    if(MutationObserver) {
      var observationTarget = document.querySelector('body'), observationConfig: MutationObserverInit;
      if(this.keyman.options['attachType'] != 'manual') { //I1961
        observationConfig = { childList: true, subtree: true};
        this.attachmentObserver = new MutationObserver(this._AutoAttachObserverCore);
        this.attachmentObserver.observe(observationTarget, observationConfig);
      }

      /**
       * Setup of handlers for dynamic detection of the kmw-disabled class tag that controls enablement.
       */
      observationConfig = { subtree: true, attributes: true, attributeOldValue: true, attributeFilter: ['class', 'readonly']};
      this.enablementObserver = new MutationObserver(this._EnablementMutationObserverCore);
      this.enablementObserver.observe(observationTarget, observationConfig);
    } else {
      console.warn("Your browser is outdated and does not support MutationObservers, a web feature " + 
        "needed by KeymanWeb to support dynamically-added elements.");
    }

    // Set exposed initialization flag to 2 to indicate deferred initialization also complete
    this.keyman.setInitialized(2);
  }

  /**
   * Initialize the desktop user interface as soon as it is ready
  **/       
  initializeUI() {
    if(this.keyman.ui && this.keyman.ui['initialize'] instanceof Function) {
      this.keyman.ui['initialize']();
      // Display the OSK (again) if enabled, in order to set its position correctly after
      // adding the UI to the page 
      this.keyman.osk._Show();     
    } else {
      window.setTimeout(this.initializeUI.bind(this),1000);
    }
  }
}