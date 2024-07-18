
import {
  CookieSerializer,
  createStyleSheet,
  getAbsoluteX,
  getAbsoluteY,
  StylesheetManager
 } from "keyman/engine/dom-utils";
import { DomEventTracker } from "keyman/engine/events";
import { BrowserConfiguration, BrowserInitOptionSpec } from "./configuration.js";
import { getStyleValue } from "./utils/getStyleValue.js";
import { AlertHost } from "./utils/alertHost.js";
import { PathConfiguration } from 'keyman/engine/paths';

/**
 * Calls document.createElement for the specified node type and also applies
 * 'user-select: none' styling to the new element.
 * @param nodeName
 * @returns
 */
export function createUnselectableElement<E extends keyof HTMLElementTagNameMap>(nodeName:E) {
  const e = document.createElement<E>(nodeName);
  e.style.userSelect="none";
  return e;
}

/**
 * Defines the base object for the long-standing `keyman.util` API methods, maintaining
 * their long-standing names and signatures as defined at
 * https://help.keyman.com/developer/engine/web/current-version/reference/util/
 */
export class UtilApiEndpoint {
  readonly config: BrowserConfiguration;
  private readonly stylesheetManager: StylesheetManager;
  private readonly domEventTracker: DomEventTracker;
  private _alertHost: AlertHost;

  constructor(config: BrowserConfiguration) {
    this.config = config;
    this.stylesheetManager = new StylesheetManager(document.body, config.applyCacheBusting);
    this.domEventTracker = new DomEventTracker();
  }

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/getAbsoluteX
   */
  public readonly getAbsoluteX = getAbsoluteX;

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/getAbsoluteY
   */
  public readonly getAbsoluteY = getAbsoluteY;

  // These four were renamed, but we need to maintain their legacy names.
  readonly _GetAbsoluteX = getAbsoluteX;
  readonly _GetAbsoluteY = getAbsoluteY;
  readonly _GetAbsolute = this.getAbsolute;

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/toNzString
   */
  public readonly toNzString = this.nzString;

  /**
   * Expose the touchable state for UIs - will disable external UIs entirely
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/isTouchDevice
   **/
  public isTouchDevice(): boolean {
    return this.config.hostDevice.touchable;
  }

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/getAbsolute
   */
  public getAbsolute(elem: HTMLElement): { x: number, y: number } {
    return {
      x: getAbsoluteX(elem),
      y: getAbsoluteY(elem)
    };
  }

  /**
   * Calls document.createElement for the specified node type and also applies
   * 'user-select: none' styling to the new element.
   * @param nodeName
   * @returns
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/createElement
   */
  public readonly createElement = createUnselectableElement;

  /**
   * Function     getOption
   * Scope        Public
   * @param       {string}    optionName  Name of option
   * @param       {*=}        dflt        Default value of option
   * @return      {*}
   * Description  Returns value of named option
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/getOption
   */
  public getOption(optionName: keyof BrowserInitOptionSpec, dflt?:any): any {
    if(optionName in this.config.paths) {
      return this.config.paths[optionName as keyof PathConfiguration];
    } else if(optionName in this.config.options) {
      return this.config.options[optionName];
    } else if(arguments.length > 1) {
      return dflt;
    } else {
      return '';
    }
  }

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/setOption
   *
   * Note:  the 'attachType' and 'ui' options currently cannot be changed via this method.
   */
  public setOption(optionName: keyof BrowserInitOptionSpec, value: any): void {
    switch(optionName) {
      case 'attachType':
        // 16.0 & before:  did nothing.
        // Fixable for 17.0 with some extra work, but the changes would likely be enough to
        // merit a focused PR.  It's not 100% straightforward.
        break;
      case 'ui':
        // 16.0 & before:  relies on the Float UI to passively pick up on any changes.
        // Only appears to be effective before the Float UI initializes.
        break;
      case 'useAlerts':
        this.config.signalUser = (value ? new AlertHost() : null);
        break;
      case 'setActiveOnRegister':
        this.config.activateFirstKeyboard = !!value;
        break;
      case 'spacebarText':
        this.config.spacebarText = value;
        break;
      default:
        throw new Error("Path-related options may not be changed after the engine has initialized.");
    }
  }

  /**
   * Document cookie parsing for use by kernel, OSK, UI etc.
   *
   * @param       {string=}       cn        cookie name (optional)
   * @return      {Object}                  array of names and strings, or array of variables and values
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/loadCookie
   */
  public loadCookie<CookieType extends Record<keyof CookieType, string | number | boolean>>(cn?: string) {
    const cookie = new CookieSerializer<CookieType>(cn);
    return cookie.load(decodeURIComponent);
  }

  /**
   * Standard cookie saving for use by kernel, OSK, UI etc.
   *
   * @param       {string}      cn            name of cookie
   * @param       {Object}      cv            object with array of named arguments and values
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/saveCookie
   */
  public saveCookie<CookieType extends Record<keyof CookieType, string | number | boolean>>(cn: string, cv: CookieType) {
    const cookie = new CookieSerializer<CookieType>(cn);
    cookie.save(cv, encodeURIComponent);
  }

  /**
   * Add a stylesheet to a page programmatically, for use by the OSK, the UI or the page creator
   *
   * @param       {string}        s             style string
   * @return      {Object}                      returns the object reference
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/addStyleSheet
   **/
  public addStyleSheet(s: string): HTMLStyleElement {
    const styleSheet = createStyleSheet(s);
    this.stylesheetManager.linkStylesheet(styleSheet);

    return styleSheet;
  }

  /**
   * Remove a stylesheet element
   *
   * @param       {Object}        s             style sheet reference
   * @return      {boolean}                     false if element is not a style sheet
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/removeStyleSheet
   **/
  public removeStyleSheet(s: HTMLStyleElement) {
    return this.stylesheetManager.unlink(s);
  }

  /**
   * Add a reference to an external stylesheet file
   *
   * @param   {string}  s   path to stylesheet file
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/linkStyleSheet
   */
  public linkStyleSheet(s: string): void {
    this.stylesheetManager.linkExternalSheet(s);
  }

  // Possible alternative:  https://www.npmjs.com/package/language-tags
  // This would necessitate linking in a npm module into compiled KeymanWeb, though.
  getLanguageCodes(lgCode: string): string[] {
    if(lgCode.indexOf('-')==-1) {
      return [lgCode];
    } else {
      return lgCode.split('-');
    }
  }

  /**
   * Function     attachDOMEvent: Note for most browsers, adds an event to a chain, doesn't stop existing events
   * Scope        Public
   * @param       {Object}    Pelem       Element (or IFrame-internal Document) to which event is being attached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True only if event to be handled on way to target element
   * Description  Attaches event handler to element DOM event
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/attachDOMEvent
   */
  attachDOMEvent<K extends keyof WindowEventMap>(
    Pelem: Window,
    Peventname: K,
    Phandler: (ev: WindowEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  attachDOMEvent<K extends keyof DocumentEventMap>(
    Pelem: Document,
    Peventname: K,
    Phandler: (ev: DocumentEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  attachDOMEvent<K extends keyof HTMLElementEventMap>(
    Pelem: HTMLElement,
    Peventname: K,
    Phandler: (ev: HTMLElementEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  public attachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (arg0: Object) => boolean, PuseCapture?: boolean): void {
    // TS can't quite track the type inference forwarding here.
    this.domEventTracker.attachDOMEvent(Pelem as any, Peventname as any, Phandler, PuseCapture);
  }

  /**
   * Function     detachDOMEvent
   * Scope        Public
   * @param       {Object}    Pelem       Element from which event is being detached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True if event was being handled on way to target element
   * Description Detaches event handler from element [to prevent memory leaks]
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/detachDOMEvent
   */
  detachDOMEvent<K extends keyof WindowEventMap>(
    Pelem: Window,
    Peventname: K,
    Phandler: (ev: WindowEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  detachDOMEvent<K extends keyof DocumentEventMap>(
    Pelem: Document,
    Peventname: K,
    Phandler: (ev: DocumentEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  detachDOMEvent<K extends keyof HTMLElementEventMap>(
    Pelem: HTMLElement,
    Peventname: K,
    Phandler: (ev: HTMLElementEventMap[K]) => any,
    PuseCapture?: boolean
  ): void;
  public detachDOMEvent(Pelem: EventTarget, Peventname: string, Phandler: (arg0: Object) => boolean, PuseCapture?: boolean): void {
    // TS can't quite track the type inference forwarding here.
    this.domEventTracker.detachDOMEvent(Pelem as any, Peventname as any, Phandler, PuseCapture);
  }

  getStyleValue = getStyleValue;

  private get alertHost(): AlertHost {
    if(this.config.alertHost) {
      return this.config.alertHost;
    } else if(!this._alertHost) {
      // Lazy init:  if KMW is set to not show alerts, we try not to initialize the alert host.
      // If the .alert API is called, though, we have no choice.
      this._alertHost = new AlertHost();
    }

    return this._alertHost;
  }

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/alert.
   */
  public alert(s: string, fn: () => void) {
    this.alertHost.alert(s, fn);
  }

  /**
   * Function     toNzString
   * Scope        Public
   * @param       {*}           item         variable to test
   * @param       {?*=}         dflt         default value
   * @return      {*}
   * Description  Test if a variable is null, false, empty string, or undefined, and return as string
   */
  public nzString(item: any, dflt: string): string {
    // // ... is this whole thing essentially just:
    // return '' + (item || dflt || '');
    // // ?

    let dfltValue = '';
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
   * Function     toNumber
   * Scope        Public
   * @param       {string}      s            numeric string
   * @param       {number}      dflt         default value
   * @return      {number}
   * Description  Return string converted to integer or default value
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/toNumber
   */
  public toNumber(s: string, dflt: number): number {
    const x = parseInt(s,10);
    return isNaN(x) ? dflt : x;
  }

  /**
   * Function     toFloat
   * Scope        Public
   * @param       {string}      s            numeric string
   * @param       {number}      dflt         default value
   * @return      {number}
   * Description  Return string converted to real value or default value
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/toFloat
   */
  public toFloat(s: string, dflt: number): number {
    const x = parseFloat(s);
    return isNaN(x) ? dflt : x;
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
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/util/rgba
   */
  public rgba(s: HTMLStyleElement, r:number, g:number, b:number, a:number): string {
    let bgColor='transparent';
    try {
      bgColor='rgba('+r+','+g+','+b+','+a+')';
    } catch(ex) {
      bgColor='rgb('+r+','+g+','+b+')';
    }

    return bgColor;
  }

  shutdown() {
    this.stylesheetManager?.unlinkAll();
    this.domEventTracker?.shutdown();
    this._alertHost?.shutdown();
  }
}