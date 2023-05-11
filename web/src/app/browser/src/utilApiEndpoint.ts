
import {
  CookieSerializer,
  createStyleSheet,
  getAbsoluteX,
  getAbsoluteY,
  StylesheetManager
 } from "keyman/engine/dom-utils";
import { DomEventTracker } from "keyman/engine/events";
import { BrowserConfiguration, BrowserInitOptionSpec } from "./configuration.js";

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

export class UtilApiEndpoint {
  readonly config: BrowserConfiguration;
  private readonly stylesheetManager: StylesheetManager;

  constructor(config: BrowserConfiguration) {
    this.config = config;
    this.stylesheetManager = new StylesheetManager(document.body, config.applyCacheBusting);
  }

  readonly getAbsoluteX = getAbsoluteX;
  readonly getAbsoluteY = getAbsoluteY;

  /**
   * Expose the touchable state for UIs - will disable external UIs entirely
   **/
  isTouchDevice(): boolean {
    return this.config.hostDevice.touchable;
  }

  getAbsolute(elem: HTMLElement): { x: number, y: number } {
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
   */
  readonly createElement = createUnselectableElement;

  /**
   * Function     getOption
   * Scope        Public
   * @param       {string}    optionName  Name of option
   * @param       {*=}        dflt        Default value of option
   * @return      {*}
   * Description  Returns value of named option
   */
  getOption(optionName: keyof BrowserInitOptionSpec, dflt?:any): any {
    if(optionName in this.config.paths) {
      return this.config.paths[optionName];
    } else if(optionName in this.config.options) {
      return this.config.options[optionName];
    } else if(arguments.length > 1) {
      return dflt;
    } else {
      return '';
    }
  }

  /**
   * Document cookie parsing for use by kernel, OSK, UI etc.
   *
   * @param       {string=}       cn        cookie name (optional)
   * @return      {Object}                  array of names and strings, or array of variables and values
   */
  loadCookie<CookieType extends Record<keyof CookieType, string | number | boolean>>(cn?: string) {
    const cookie = new CookieSerializer<CookieType>(cn);
    return cookie.load(decodeURIComponent);
  }

  /**
   * Standard cookie saving for use by kernel, OSK, UI etc.
   *
   * @param       {string}      cn            name of cookie
   * @param       {Object}      cv            object with array of named arguments and values
   */
  saveCookie<CookieType extends Record<keyof CookieType, string | number | boolean>>(cn: string, cv: CookieType) {
    const cookie = new CookieSerializer<CookieType>(cn);
    cookie.save(cv, encodeURIComponent);
  }

  /**
   * Add a stylesheet to a page programmatically, for use by the OSK, the UI or the page creator
   *
   * @param       {string}        s             style string
   * @return      {Object}                      returns the object reference
   **/
  addStyleSheet(s: string): HTMLStyleElement {
    const styleSheet = createStyleSheet(s);
    this.stylesheetManager.linkStylesheet(styleSheet);

    return styleSheet;
  }

  /**
   * Remove a stylesheet element
   *
   * @param       {Object}        s             style sheet reference
   * @return      {boolean}                     false if element is not a style sheet
   **/
  removeStyleSheet(s: HTMLStyleElement) {
    return this.stylesheetManager.unlink(s);
  }

  /**
   * Add a reference to an external stylesheet file
   *
   * @param   {string}  s   path to stylesheet file
   */
  linkStyleSheet(s: string): void {
    this.stylesheetManager.linkExternalSheet(s);
  }

  shutdown() {
    this.stylesheetManager?.unlinkAll();
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

  readonly DomEventTracker = DomEventTracker;
}