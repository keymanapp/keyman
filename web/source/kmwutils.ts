// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Includes the Device definition set.
/// <reference path="kmwdevice.ts" />
// Includes the DOM utils, since our UI modules need access to certain methods here.
/// <reference path="dom/utils.ts" />

namespace com.keyman {
  class DOMEventTracking {
    Pelem: EventTarget;
    Peventname: string;
    Phandler: (Object) => boolean;
    PuseCapture?: boolean

    constructor(Pelem: EventTarget, Peventname: string, Phandler: (Object) => boolean, PuseCapture?: boolean) {
      this.Pelem = Pelem;
      this.Peventname = Peventname.toLowerCase();
      this.Phandler = Phandler;
      this.PuseCapture = PuseCapture;
    }

    equals(other: DOMEventTracking): boolean {
      return this.Pelem == other.Pelem && this.Peventname == other.Peventname &&
        this.Phandler == other.Phandler && this.PuseCapture == other.PuseCapture;
    }
  };

  export class Util {
    // Generalized component event registration
    device: Device;
    activeDevice: Device;
    physicalDevice: Device;

    linkedStylesheets: (HTMLLinkElement|HTMLStyleElement)[] = [];

    waiting: HTMLDivElement;                  // The element displayed for util.wait and util.alert.

    // An object mapping event names to individual event lists.  Maps strings to arrays.
    private events: { [name: string]: ((Object) => boolean)[];} = {};
    private currentEvents: string[] = [];  // The event messaging call stack.

    private domEvents: DOMEventTracking[] = [];

    private embeddedFonts: any[] = [];     // Array of currently embedded font descriptor entries.  (Is it just a string?)

    // Consider refactoring keymanweb.options to within Util.

    private keyman: KeymanBase; // Closure doesn't like relying on the global object from within a class def.

    constructor(keyman: any) {
      this.initDevices();

      this.keyman = keyman;
    }

    // Possible alternative:  https://www.npmjs.com/package/language-tags
    // This would necessitate linking in a npm module into compiled KeymanWeb, though.
    ['getLanguageCodes'](lgCode: string): string[] {
      if(lgCode.indexOf('-')==-1) {
        return [lgCode];
      } else {
        return lgCode.split('-');
      }
    }

    initDevices(): void {
      this.device = new Device();
      this.physicalDevice = new Device();
      this.activeDevice = this.device;

      // Initialize the true device values.
      this.device.detect();

      /* DEBUG: Force touch device   (Build 360)

      device.touchable = true;
      device.browser = 'safari';
      device.formFactor = 'tablet';
      device.OS = 'iOS';

      END DEBUG */

      /* If we've made it to this point of initialization and aren't anything else, KeymanWeb assumes
      * we're a desktop.  Since we don't yet support desktops with touch-based input, we disable it here.
      */
      if(this.device.formFactor == 'desktop') {
        this.device.touchable = false;
      }

      /**
       * Represents hardware-based keystrokes regardless of the 'true' device, facilitating hardware keyboard input
       * whenever touch-based input is available.
       */
      this.physicalDevice = new Device();
      this.physicalDevice.touchable = false;
      this.physicalDevice.browser = this.device.browser;
      this.physicalDevice.formFactor = 'desktop';
      this.physicalDevice.OS = this.device.OS;
    }

    /**
     * Function     arrayFromNodeList
     * Scope        Public
     * @param       {Object}    nl a node list, as returned from getElementsBy_____ methods.
     * Description  Transforms a node list into an array.   *
     * @return      {Array<Element>}
     */
    arrayFromNodeList(nl: NodeList|HTMLCollectionOf<Element>): HTMLElement[] {
      var res = [];
      for(var i=0; i < nl.length; i++) {
        res.push(nl[i]);
      }
      return res;
    }

    /**
     * Function    addEventListener
     * Scope       Private
     * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
     * @param      {function(Object)}   func      event handler
     * @return     {boolean}
     * Description Add (or replace) an event listener for this component
     */
    addEventListener(event: string, func: (Object) => boolean): boolean {
      this.removeEventListener(event, func);
      this.events[event].push(func);
      return true;
    }

    /**
     * Function    removeEventListener
     * Scope       Private
     * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
     * @param      {function(Object)}   func      event handler
     * @return     {boolean}
     * Description Remove the specified function from the listeners for this event
     */
    removeEventListener(event: string, func: (Object) => boolean): boolean {
      if(typeof this.events[event] == 'undefined') {
        this.events[event] = [];
      }

      for(var i=0; i<this.events[event].length; i++) {
        if(this.events[event][i] == func) {
          this.events[event].splice(i, 1);
          return true;
        }
      }
      return false;
    }

    /**
     * Function    callEvent
     * Scope       Private
     * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
     * @param      {Array}      params    parameter array for function
     * @return     {boolean}
     * Description Invoke an event using any function with up to four arguments
     */
    callEvent(event: string, params: Object|Object[]): boolean {
      if(typeof this.events[event] == 'undefined') {
        return true;
      }

      if(this.currentEvents.indexOf(event) != -1) {
        return false;  // Avoid event messaging recursion!
      }

      this.currentEvents.push(event);

      for(var i=0; i<this.events[event].length; i++) {
        var func=this.events[event][i], result=false;
        try {
          result=func(params);
        } catch(strExcept) {
          console.error(strExcept);
          result=false;
        } //don't know whether to use true or false here
        if(result === false) {
          this.currentEvents.pop();
          return false;
        }
      }
      this.currentEvents.pop();
      return true;
    }

    /**
     * Function     attachDOMEvent: Note for most browsers, adds an event to a chain, doesn't stop existing events
     * Scope        Public
     * @param       {Object}    Pelem       Element (or IFrame-internal Document) to which event is being attached
     * @param       {string}    Peventname  Name of event without 'on' prefix
     * @param       {function(Object)}  Phandler    Event handler for event
     * @param       {boolean=}  PuseCapture True only if event to be handled on way to target element
     * Description  Attaches event handler to element DOM event
     */
    attachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (Object) => boolean, PuseCapture?: boolean): void {
      this.detachDOMEvent(Pelem, Peventname, Phandler, PuseCapture);
      Pelem.addEventListener(Peventname, Phandler, PuseCapture?true:false);

      // Since we're attaching to the DOM, these events should be tracked for detachment during shutdown.
      var event = new DOMEventTracking(Pelem, Peventname, Phandler, PuseCapture);
      this.domEvents.push(event);
    }

    /**
     * Function     detachDOMEvent
     * Scope        Public
     * @param       {Object}    Pelem       Element from which event is being detached
     * @param       {string}    Peventname  Name of event without 'on' prefix
     * @param       {function(Object)}  Phandler    Event handler for event
     * @param       {boolean=}  PuseCapture True if event was being handled on way to target element
     * Description Detaches event handler from element [to prevent memory leaks]
     */
    detachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (Object) => boolean, PuseCapture?: boolean): void {
      Pelem.removeEventListener(Peventname, Phandler, PuseCapture);

      // Since we're detaching, we should drop the tracking data from the old event.
      var event = new DOMEventTracking(Pelem, Peventname, Phandler, PuseCapture);
      for(var i = 0; i < this.domEvents.length; i++) {
        if(this.domEvents[i].equals(event)) {
          this.domEvents.splice(i, 1);
          break;
        }
      }
    }

    /**
     * Function     getOption
     * Scope        Public
     * @param       {string}    optionName  Name of option
     * @param       {*=}        dflt        Default value of option
     * @return      {*}
     * Description  Returns value of named option
     */
    getOption(optionName:string, dflt:any): any {
      if(optionName in this.keyman.options) {
        return this.keyman.options[optionName];
      } else if(arguments.length > 1) {
        return dflt;
      } else {
        return '';
      }
    }

    /**
     * More reliable way of identifying  element class
     * @param   {Object}  e HTML element
     * @param   {string}  name  class name
     * @return  {boolean}
     */
    hasClass(e: HTMLElement, name: string): boolean {
      var className = " " + name + " ";
      return (" " + e.className + " ").replace(/[\n\t\r\f]/g, " ").indexOf(className) >= 0;
    }

    /**
     * Function     setOption
     * Scope        Public
     * @param       {string}    optionName  Name of option
     * @param       {*=}        value       Value of option
     * Description  Sets value of named option
     */
    setOption(optionName,value) {
      this.keyman.options[optionName] = value;
    }

    //  Unofficial API used by our desktop UIs.
    getAbsoluteX(Pobj: HTMLElement): number {
      return dom.Utils.getAbsoluteX(Pobj);
    }

    //  Unofficial API used by our desktop UIs.
    getAbsoluteY(Pobj: HTMLElement): number {
      return dom.Utils.getAbsoluteY(Pobj);
    }

    /**
     * Function     getAbsolute
     * Scope        Public
     * @param       {Object}    Pobj        HTML element
     * @return      {Object.<string,number>}
     * Description  Returns absolute position of Pobj element with respect to page
     */
    getAbsolute(Pobj: HTMLElement) {
      var p={
        /* @ export */
        x: this.getAbsoluteX(Pobj),
        /* @ export */
        y: this.getAbsoluteY(Pobj)
      };
      return p;
    }

    //  Unofficial API used by our desktop UIs.
    _GetAbsolute = this.getAbsolute;

    /**
     * Select start handler (to replace multiple inline handlers) (Build 360)
     */
    selectStartHandler = function() {
      return false;
    }

    /**
     * Default mouse down event handler (to replace multiple inline handlers) (Build 360)
     */
    mouseDownPreventDefaultHandler(e: MouseEvent) {
      if(e) {
        e.preventDefault();
      }
    }

    // Found a bit of magic formatting that allows dynamic return typing for a specified element tag!
    _CreateElement<E extends "p"|"style"|"script"|"div"|"canvas"|"span">(nodeName:E) {
      var e = document.createElement<E>(nodeName);

      // Make element unselectable (Internet Explorer)
      if (typeof e.onselectstart != 'undefined') { //IE route
        e.onselectstart=this.selectStartHandler; // Build 360
      } else { // And for well-behaved browsers (may also work for IE9+, but not necessary)
        e.style.MozUserSelect="none";
        e.style.KhtmlUserSelect="none";
        e.style.UserSelect="none";
        e.style.WebkitUserSelect="none";
      }
      return e;
    }

        /**
     * Function     _CancelMouse
     * Scope        Private
     * @param       {Object}      e     event
     * @return      {boolean}           always false
     * Description  Closes mouse click event
     */
    _CancelMouse=function(e: MouseEvent) {
      e = com.keyman.singleton._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(e && e.preventDefault) {
        e.preventDefault();
      }
      if(e) {
        e.cancelBubble=true;
        e.returnValue=false;
      } // I2409 - Avoid focus loss for visual keyboard events

      return false;
    }

    createElement = this._CreateElement;

    /**
     * Function     getIEVersion
     * Scope        Public
     * @return      {number}
     * Description  Return IE version number (or 999 if browser not IE)
     */
    getIEVersion() {
      return Device._GetIEVersion();
    }

    getFontSizeStyle(e: HTMLElement|string): {val: number, absolute: boolean} {
      var val: number;
      var fs: string;

      if(typeof e == 'string') {
        fs = e;
      } else {
        fs = e.style.fontSize;
        if(!fs) {
          fs = getComputedStyle(e).fontSize;
        }
      }

      if(fs.indexOf('em') != -1) {
        val = parseFloat(fs.substr(0, fs.indexOf('em')));
        return {val: val, absolute: false};
      } else if(fs.indexOf('px') != -1) {
        val = parseFloat(fs.substr(0, fs.indexOf('px')));
        return {val: val, absolute: true};
      } else if(fs.indexOf('pt') != -1) {
        // 16 px ~= 12 pt.
        // Reference: https://kyleschaeffer.com/css-font-size-em-vs-px-vs-pt-vs-percent
        val = parseFloat(fs.substr(0, fs.indexOf('pt')));
        return {val: (4 * val / 3), absolute: true};
      } else if(fs.indexOf('%') != -1) {
        val = parseFloat(fs.substr(0, fs.indexOf('%')));
        return {val: val/100, absolute: false};
      } else if(!isNaN(val = Number(fs))) {
        // Note:  this one is NOT natively handled by browsers!
        //        We'll treat it as if it were 'pt', since that's likely the user's
        //        most familiar font size unit.
        return {val: (4 * val / 3), absolute: true};
      } else {
        // Cannot parse.
        console.error("Could not properly parse specified fontsize info: '" + fs + "'.");
        return null;
      }
    }

    /**
     * Get browser-independent computed style value for element
     *
     * @param       {Element}     e             HTML element
     * @param       {string}      s             CSS style name
     * @return      {*}
     */
    getStyleValue(e:HTMLElement, s:string) {
      // Build 349: error trap added, since on iOS, getPropertyValue may fail
      // and crash in some cases, possibly if passed a text node
      try
      {
        if(e && (typeof(window.getComputedStyle) != 'undefined')) {
            return window.getComputedStyle(e,'').getPropertyValue(s);
        }
      }
      catch(ex){}

      // Return empty string if unable to get style value
      return '';
    }

    /**
     * Get browser-independent computed style integer value for element  (Build 349)
     *
     * @param       {Element}     e             HTML element
     * @param       {string}      s             CSS style name
     * @param       {number=}     d             default value if NaN
     * @return      {number}                    integer value of style
     */
    getStyleInt(e: HTMLElement, s: string, d?: number): number {
      var x=parseInt(this.getStyleValue(e,s),10);
      if(!isNaN(x)) {
        return x;
      }

      // Return the default value if numeric, else 0
      if(typeof(d) == 'number') {
        return d;
      } else {
        return 0;
      }
    }

    /**
     * Expose the touchable state for UIs - will disable external UIs entirely
     **/
    isTouchDevice(): boolean {
      return this.device.touchable;
    }

    /**
     * Get orientation of tablet or phone  display
     *
     * @return      {boolean}
     */
    portraitView() { // new for I3363 (Build 301)
      return !this.landscapeView();
    }

    /**
     * Get orientation of tablet or phone  display
     *
     * @return      {boolean}
     */
    landscapeView(): boolean	{ // new for I3363 (Build 301)
      var orientation: number;

      // Assume portrait mode if orientation undefined
      if(typeof window.orientation != 'undefined') { // Used by iOS Safari
        // Else landscape for +/-90, portrait for 0, +/-180
        orientation = window.orientation as number;
      } else if(typeof window.screen.orientation != 'undefined') { // Used by Firefox, Chrome
        orientation = window.screen.orientation.angle;
      }

      if(orientation !== undefined) {
        return (Math.abs(orientation/90) == 1);
      } else {
        return false;
      }
    }

    /**
     * Get viewport scale factor for this document
     *
     * @return      {number}
     */
    getViewportScale(): number {
      // This can sometimes fail with some browsers if called before document defined,
      // so catch the exception
      try {
        // Get viewport width
        var viewportWidth = document.documentElement.clientWidth;

        // Return a default value if screen width is greater than the viewport width (not fullscreen).
        if(screen.width > viewportWidth) {
          return 1;
        }

        // Get the orientation corrected screen width
        var screenWidth = screen.width;
        if(this.landscapeView()) {
          // Take larger of the two dimensions
          if(screen.width < screen.height) {
            screenWidth = screen.height;
          }
        } else {
          // Take smaller of the two dimensions
          if(screen.width > screen.height) {
            screenWidth = screen.height;
          }
        }
        // Calculate viewport scale
        return Math.round(100*screenWidth / window.innerWidth)/100;
      } catch(ex) {
        return 1;
      }
    }

    /**
     * Return height of URL bar on mobile devices, if visible
     * TODO: This does not seem to be right, so is not currently used
     *
     * @return      {number}
     */
    barHeight(): number {
      var dy=0;
      if(this.device.formFactor == 'phone') {
        dy=screen.height/2-window.innerHeight-(this.landscapeView() ? this.device.dyLandscape: this.device.dyPortrait);
      }
      return dy;
    }

    /**
     * Function     _EncodeEntities
     * Scope        Private
     * @param       {string}      P_txt         string to be encoded
     * @return      {string}                    encoded (html-safe) string
     * Description Encode angle brackets and ampersand in text string
     */
    _EncodeEntities(P_txt: string): string {
      return P_txt.replace('&','&amp;').replace('<','&lt;').replace('>','&gt;');  // I1452 part 2
    }

    /**
     * Function     createShim
     * Scope        Public
     * Description  [Deprecated] Create an IFRAME element to go between KMW and drop down (to fix IE6 bug)
     * @deprecated
     */
    createShim(): void {    // I1476 - Handle SELECT overlapping BEGIN
      console.warn("The util.createShim function is deprecated, as its old functionality is no longer needed.  " +
        "It and references to its previously-produced shims may be safely removed.");
      return;
    }

    // I1476 - Handle SELECT overlapping BEGIN

    /**
     * Function     showShim
     * Scope        Public
     * @param       {Object}      Pvkbd         Visual keyboard DIV element
     * @param       {Object}      Pframe        IFRAME shim element
     * @param       {Object}      Phelp         OSK Help DIV element
     * Description  [Deprecated] Display iFrame under OSK at its currently defined position, to allow OSK to overlap SELECT elements (IE6 fix)
     * @deprecated
     */
    showShim(Pvkbd: HTMLElement, Pframe: HTMLElement, Phelp: HTMLElement) {
      console.warn("The util.showShim function is deprecated, as its old functionality is no longer needed.  It may be safely removed.");
    }

    /**
     * Function     hideShim
     * Scope        Public
     * @param       {Object}      Pframe        IFRAME shim element
     * Description  [Deprecated] Hide iFrame shim containing OSK
     * @deprecated
     */
    hideShim(Pframe: HTMLElement) {
      console.warn("The util.hideShim function is deprecated, as its old functionality is no longer needed.  It may be safely removed.");
    }

    /**
     * Function     rgba
     * Scope        Public
     * @param       {Object}      s           element style object
     * @param       {number}      r           red value, 0-255
     * @param       {number}      g           green value, 0-255
     * @param       {number}      b           blue value, 0-255
     * @param       {number}      a           opacity value, 0-1.0
     * @return      {string}                  background colour style string
     * Description  Browser-independent alpha-channel management
     */
    rgba(s: HTMLStyleElement, r:number, g:number, b:number, a:number): string {
      var bgColor='transparent';
      try {
        bgColor='rgba('+r+','+g+','+b+','+a+')';
      } catch(ex) {
        bgColor='rgb('+r+','+g+','+b+')';
      }

      return bgColor;
    }

    /**
     * Add a stylesheet to a page programmatically, for use by the OSK, the UI or the page creator
     *
     * @param       {string}        s             style string
     * @return      {Object}                      returns the object reference
     **/
    addStyleSheet(s: string): HTMLStyleElement {
      var _ElemStyle: HTMLStyleElement = <HTMLStyleElement>document.createElement<'style'>('style');

      _ElemStyle.type = 'text/css';
      _ElemStyle.appendChild(document.createTextNode(s));

      var _ElemHead=document.getElementsByTagName('HEAD');
      if(_ElemHead.length > 0) {
        _ElemHead[0].appendChild(_ElemStyle);
      } else {
        document.body.appendChild(_ElemStyle); // Won't work on Chrome, ah well
      }

      this.linkedStylesheets.push(_ElemStyle);

      return _ElemStyle;
    }

    /**
     * Remove a stylesheet element
     *
     * @param       {Object}        s             style sheet reference
     * @return      {boolean}                     false if element is not a style sheet
     **/
    removeStyleSheet(s: HTMLStyleElement) {
      if(s == null || typeof(s) != 'object') {
        return false;
      }

      if(s.nodeName != 'STYLE') {
        return false;
      }

      if(typeof(s.parentNode) == 'undefined' || s.parentNode == null) {
        return false;
      }

      s.parentNode.removeChild(s);
      return true;
    }

    /**
     * Add a reference to an external stylesheet file
     *
     * @param   {string}  s   path to stylesheet file
     */
    linkStyleSheet(s: string): void {
      var headElements=document.getElementsByTagName('head');
      if(headElements.length > 0) {
        var linkElement=document.createElement('link');
        linkElement.type='text/css';
        linkElement.rel='stylesheet';
        linkElement.href=s;
        this.linkedStylesheets.push(linkElement);
        headElements[0].appendChild(linkElement);
      }
    }

    /**
     * Add a stylesheet with a font-face CSS descriptor for the embedded font appropriate
     * for the browser being used
     *
     * @param    {Object}  fd  keymanweb font descriptor
     **/
    addFontFaceStyleSheet(fd: any) { // TODO:  Font descriptor object needs definition!
      // Test if a valid font descriptor
      if(typeof(fd) == 'undefined') return;

      if(typeof(fd['files']) == 'undefined') fd['files']=fd['source'];
      if(typeof(fd['files']) == 'undefined') return;

      var i,ttf='',woff='',eot='',svg='',fList=[];

  // TODO: 22 Aug 2014: check that font path passed from cloud is actually used!

      // Do not add a new font-face style sheet if already added for this font
      for(i=0; i<this.embeddedFonts.length; i++) {
        if(this.embeddedFonts[i] == fd['family']) {
          return;
        }
      }

      if(typeof(fd['files']) == 'string') {
        fList[0]=fd['files'];
      } else {
        fList=fd['files'];
      }

      for(i=0;i<fList.length;i++) {
        if(fList[i].toLowerCase().indexOf('.otf') > 0) ttf=fList[i];
        if(fList[i].toLowerCase().indexOf('.ttf') > 0) ttf=fList[i];
        if(fList[i].toLowerCase().indexOf('.woff') > 0) woff=fList[i];
        if(fList[i].toLowerCase().indexOf('.eot') > 0) eot=fList[i];
        if(fList[i].toLowerCase().indexOf('.svg') > 0) svg=fList[i];
      }

      // Font path qualified to support page-relative fonts (build 347)
      if(ttf != '' && (ttf.indexOf('/') < 0))  {
        ttf = this.keyman.options['fonts']+ttf;
      }

      if(woff != '' && (woff.indexOf('/') < 0)) {
        woff = this.keyman.options['fonts']+woff;
      }

      if(eot != '' && (eot.indexOf('/') < 0)) {
      eot = this.keyman.options['fonts']+eot;
      }

      if(svg != '' && (svg.indexOf('/') < 0)) {
        svg = this.keyman.options['fonts']+svg;
      }

      // Build the font-face definition according to the browser being used
      var s='@font-face {\nfont-family:'
        +fd['family']+';\nfont-style:normal;\nfont-weight:normal;\n';

      // Detect if Internet Explorer and version if so
      var IE=Device._GetIEVersion();

      // Build the font source string according to the browser,
      // but return without adding the style sheet if the required font type is unavailable

      // Modern browsers: use WOFF, TTF and fallback finally to SVG. Don't provide EOT
      if(IE >= 9) {
        if(this.device.OS == 'iOS') {
          if(ttf != '') {
            // Modify the url if required to prevent caching
            ttf = this.unCached(ttf);
            s=s+'src:url(\''+ttf+'\') format(\'truetype\');';
          } else {
            return;
          }
        } else {
          var s0 = [];

          if(this.device.OS == 'Android') {
            // Android 4.2 and 4.3 have bugs in their rendering for some scripts
            // with embedded ttf or woff.  svg mostly works so is a better initial
            // choice on the Android browser.
            if(svg != '') {
              s0.push("url('"+svg+"') format('svg')");
            }

            if(woff != '') {
              s0.push("url('"+woff+"') format('woff')");
            }

            if(ttf != '') {
              s0.push("url('"+ttf+"') format('truetype')");
            }
          } else {
            if(woff != '') {
              s0.push("url('"+woff+"') format('woff')");
            }

            if(ttf != '') {
              s0.push("url('"+ttf+"') format('truetype')");
            }

            if(svg != '') {
              s0.push("url('"+svg+"') format('svg')");
            }
          }

          if(s0.length == 0) {
            return;
          }

          s += 'src:'+s0.join(',')+';';
        }
      } else { // IE 6-8
        if(eot != '') {
          s=s+'src:url(\''+eot+'\');';
        } else {
          return;
        }
      }

      s=s+'\n}\n';

      this.addStyleSheet(s);
      this.embeddedFonts.push(fd['family']);
    }

    /**
     * Allow forced reload if necessary (stub only here)
     *
     *  @param  {string}  s unmodified URL
     *  @return {string}    modified URL
     */
    unCached(s: string): string {
      // var t=(new Date().getTime());
      // s = s + '?v=' + t;
      return s;
    }

    /**
     * Document cookie parsing for use by kernel, OSK, UI etc.
     *
     * @param       {string=}       cn        cookie name (optional)
     * @return      {Object}                  array of names and strings, or array of variables and values
     */
    loadCookie(cn?: string) {
      var v={};
      if(arguments.length > 0) {
        var cx = this.loadCookie();
        for(var t in cx) {
          if(t == cn) {
            var d = decodeURIComponent(cx[t]).split(';');
            for(var i=0; i<d.length; i++) {
              var xc = d[i].split('=');
              if(xc.length > 1) {
                v[xc[0]] = xc[1];
              } else {
                v[xc[0]] = '';
              }
            }
          }
        }
      } else {
        if(typeof(document.cookie) != 'undefined' && document.cookie != '') {
          var c = document.cookie.split(/;\s*/);
          for(var i = 0; i < c.length; i++) {
            var d = c[i].split('=');
            if(d.length == 2) {
              v[d[0]] = d[1];
            }
          }
        }
      }
      return v;
    }

    /**
     * Standard cookie saving for use by kernel, OSK, UI etc.
     *
     * @param       {string}      cn            name of cookie
     * @param       {Object}      cv            object with array of named arguments and values
     */
    saveCookie(cn: string, cv) {
      var s='';
      for(var v in cv) {
        s = s + v+'='+cv[v]+";";
      }

      var d = new Date(new Date().valueOf() + 1000 * 60 * 60 * 24 * 30).toUTCString();
      document.cookie = cn+'='+encodeURIComponent(s)+'; path=/; expires='+d;//Fri, 31 Dec 2099 23:59:59 GMT;';
    }

    /**
     * Function     toNumber
     * Scope        Public
     * @param       {string}      s            numeric string
     * @param       {number}      dflt         default value
     * @return      {number}
     * Description  Return string converted to integer or default value
     */
    toNumber(s: string, dflt: number): number {
      var x = parseInt(s,10);
      return isNaN(x) ? dflt : x;
    }

    /**
     * Function     toNumber
     * Scope        Public
     * @param       {string}      s            numeric string
     * @param       {number}      dflt         default value
     * @return      {number}
     * Description  Return string converted to real value or default value
     */
    toFloat(s: string, dflt: number): number {
      var x = parseFloat(s);
      return isNaN(x) ? dflt : x;
    }

    /**
     * Function     toNzString
     * Scope        Public
     * @param       {*}           item         variable to test
     * @param       {?*=}         dflt         default value
     * @return      {*}
     * Description  Test if a variable is null, false, empty string, or undefined, and return as string
     */
    nzString(item: any, dflt: any): string {
      var dfltValue = '';
      if(arguments.length > 1) {
        dfltValue = dflt;
      }

      if(typeof(item) == 'undefined') {
        return dfltValue;
      }

      if(item == null) {
        return dfltValue;
      }

      if(item == 0 || item == '') {
        return dfltValue;
      }

      return ''+item;
    }

    /**
     * Return the event target for any browser
     *
     * @param       {Event}      e        event
     * @return      {Object}              HTML element
     */
    eventTarget(e: Event): EventTarget {
      if(!e) {
        return null;
      } else if (e.target) {       // most browsers
        return e.target;
      } else if (e.srcElement) {
        return e.srcElement;
      } else if(window.event) { //IE 8 (and earlier)
        return window.event.srcElement;
      } else {
        return null;            // shouldn't happen!
      }
    }

    /**
     * Return the event type for any browser
     *
     * @param       {Event}      e        event
     * @return      {string}              type of event
     */
    eventType(e: Event): string {
      if(e && e.type) {         // most browsers
        return e.type;
      } else if(window.event) { // IE 8 (and earlier)
        return window.event.type;
      } else {
        return '';              // shouldn't happen!
      }
    }

    /**
     * Customized alert
     *
     * @param     {string}        s       alert text
     * @param     {function()=}   fn      function to call when alert dismissed
     */
    alert(s: string, fn?: () => void): void {
      var bg = this.waiting, nn=bg.firstChild.childNodes;
      (nn[0] as HTMLElement).style.display='block';
      (nn[1] as HTMLElement).className='kmw-alert-text';
      (nn[1] as HTMLElement).innerHTML=s;
      (nn[2] as HTMLElement).style.display='none';
      bg.style.display='block';
      if(arguments.length > 1) {
        bg.dismiss=fn;
      } else {
        bg.dismiss=null;
      }
    }

    // Stub definition to be fleshed out depending upon native/embedded mode.
    wait(s: string|boolean): void {

    }

    /**
     *  Prepare the background and keyboard loading wait message box
     *  Should not be called before options are defined during initialization
     **/
    prepareWait(): void {
      var bg: HTMLDivElement = <HTMLDivElement>document.createElement('DIV'),
          lb=document.createElement('DIV'),
          lt=document.createElement('DIV'),
          gr=document.createElement('DIV'),
          bx=document.createElement('DIV');

      bg.className='kmw-wait-background';
      lb.className='kmw-wait-box';
      bg.dismiss=null;
      lt.className='kmw-wait-text';
      gr.className='kmw-wait-graphic';
      bx.className='kmw-alert-close';

      // Close alert if anywhere in box is touched, since close box is too small on mobiles
      lb.onmousedown=lb.onclick=function(e) {
        // Ignore if waiting, only handle for alert
        if(bx.style.display == 'block') {
          bg.style.display='none';
          if(bg.dismiss) {
            bg.dismiss();
          }
        }
      };

      lb.addEventListener('touchstart', lb.onclick, false);
      bg.onmousedown=bg.onclick=function(e) {
        e.preventDefault();
        e.stopPropagation();

      }
      bg.addEventListener('touchstart', bg.onclick, false);
      lb.appendChild(bx);
      lb.appendChild(lt);
      lb.appendChild(gr);
      bg.appendChild(lb);
      document.body.appendChild(bg);
      this.waiting=bg;
    }

    shutdown() {
      // Remove all event-handler references rooted in KMW events.
      this.events = {};

      // Remove all events linking to elements of the original, unaltered page.
      // This should sever any still-existing page ties to this instance of KMW,
      // allowing browser GC to do its thing.
      for(let event of this.domEvents) {
        this.detachDOMEvent(event.Pelem, event.Peventname, event.Phandler, event.PuseCapture);
      }

      // Remove any KMW-added DOM element clutter.
      this.waiting.parentNode.removeChild(this.waiting);

      for(let ss of this.linkedStylesheets) {
        if(ss.remove) {
          ss.remove();
        } else if(ss.parentNode) {
          ss.parentNode.removeChild(ss);
        }
      }
    }

    /**
     * Get path of keymanweb script, for relative references
     *
     * *** This is not currently used, but may possibly be needed if ***
     * *** script identification during loading proves unreliable.   ***
     *
     *  @param    {string}      sName   filename prefix
     *  @return   {string}      path to source, with trailing slash
    **/
    myPath(sName: string): string {
      var i, scripts=document.getElementsByTagName('script'), ss;

      for(i=0; i<scripts.length; i++) {
        ss=scripts[i];
        if(ss.src.indexOf(sName) >= 0) {
          return ss.src.substr(0,ss.src.lastIndexOf('/')+1);
        }
      }

      return '';
    }

    // Prepend the appropriate protocol if not included in path
    prependProtocol(path: string): string {
      var pattern = new RegExp('^https?:');

      if(pattern.test(path)) {
        return path;
      } else if(path.substr(0,2) == '//') {
        return this.keyman.protocol+path;
      } else if(path.substr(0,1) == '/') {
        return this.keyman.protocol+'/'+path;
      } else {
        return this.keyman.protocol+'//'+path;
      }
    }

    /**
     * Return the appropriate test string for a given font
     *
     * TODO: Tidy up and remove arrays once 'sample' included in font metadata
     *
     *  @param  {Object}    fd    font meta-data object
     *  @return {string}          string to compare width
     *
     */
    testString(fd): string {
      var fontName=fd['family'],
      i,s='BESbswy';

      if('sample' in fd && typeof(fd['sample']) == 'string') {
        return s+fd['sample'];
      }

      var f=['TamilWeb','TibetanWeb','LatinWeb','CherokeeWeb',
            'EgyptianWeb','SinhalaWeb','KhmerWeb','ArabicWeb',
            'BurmeseWeb','LaoWeb','OriyaWeb','GeezWeb'],
          t=['\u0BBE\u0BF5','\u0F7F\u0FD0','\u02B0\u02A4','\u13D0\u13C9',
            '\uA723\uF7D3','\u0DD8\u0DA3','\u17D6\u178E','\u0639\u06B3',
            '\u1038\u1024','\u0EC0\u0EDD','\u0B03\u0B06','\u1361\u132C'];

      for(i=0; i<f.length; i++) {
        if(fontName == f[i]) {
          return s+t[i];
        }
      }

      return s;
    }

    /**
     * Test if a font is installed (or available) on the target platform
     *
     * @param       {Object}        fd    font structure
     * @return      {boolean}             true if font available
     */
    checkFont(fd): boolean {
      var fontReady=false, fontName=fd['family'];

      // Create an absolute positioned div and two paragraph elements with spans for the test string.
      // The paragraph elements ensure that the spans are measured from the same point, otherwise
      // pixel rounding can result in different widths for the same string and styles.
      // Using a separate invisible DIV is more reliable than other positioning.
      var d=document.createElement('DIV'), ds=d.style,
          p1=document.createElement('P'),
          p2=document.createElement('P'),
          t1=document.createElement('SPAN'), s1=t1.style,
          t2=document.createElement('SPAN'), s2=t2.style;

      ds.position='absolute';
      ds.top='10px';
      ds.left='10px';
      ds.visibility='hidden';
      document.body.appendChild(d);
      d.appendChild(p1);
      d.appendChild(p2);
      p1.appendChild(t1);
      p2.appendChild(t2);

      // Firefox fails without the !important prefix on the fallback font,
      // apparently applying the same font to both elements.
      // But it also fails to distinguish the two if !important is added to the test font!
      // *** TODO: See if still true after changes Dec 2013 ***
      // Must apply !important tag to font-family, but must apply it to the CSS style, not the JS object member
      // c.f. http://stackoverflow.com/questions/462537/overriding-important-style-using-javascript
      t1.setAttribute('style','font-family:monospace !important');
      s2.fontFamily=fontName+',monospace';
      s1.fontSize=s2.fontSize='24px';      // Not too large, to avoid wrapping or overflow

      // Include narrow and wide characters from each unique script
      t1.innerHTML=t2.innerHTML=this.testString(fd);

      // Compare the actual width of each span. Checking monospace, serif,
      // and sans-serif helps to avoid falsely reporting the font as ready
      // The width must be different for all three tests.
      if(t1.offsetWidth != t2.offsetWidth) {
        t1.setAttribute('style','font-family:sans-serif !important');
        s2.fontFamily=fontName+',sans-serif';
        if(t1.offsetWidth != t2.offsetWidth) {
          t1.setAttribute('style','font-family:serif !important');
          s2.fontFamily=fontName+',serif';
        }
      }

      fontReady=(t1.offsetWidth != t2.offsetWidth);

      // Delete test elements
      p1.removeChild(t1);
      p2.removeChild(t2);
      d.removeChild(p1);
      d.removeChild(p2);
      document.body.removeChild(d);

      return fontReady;
    }

    /**
     * Check a font descriptor for font availability, returning true if undefined
     *
     *  @param  {Object}  fd  font descriptor member of keyboard stub
     *  @return {boolean}
     **/
    checkFontDescriptor(fd): boolean {
      if(typeof(fd) == 'undefined' || typeof(fd['family']) != 'string') {
        return true;
      }

      return this.checkFont(fd);
    }
  }
}

import Util = com.keyman.Util;