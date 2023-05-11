
import {
  CookieSerializer,
  createStyleSheet,
  getAbsoluteX,
  getAbsoluteY,
  StylesheetManager
 } from "keyman/engine/dom-utils";
import { BrowserConfiguration, BrowserInitOptionSpec } from "./configuration.js";

export function createUnselectableElement<E extends keyof HTMLElementTagNameMap>(nodeName:E) {
  const e = document.createElement<E>(nodeName);
  e.style.userSelect="none";
  return e;
}

export class UtilApiEndpoint {
  readonly config: BrowserConfiguration;

  constructor(config: BrowserConfiguration) {
    this.config = config;
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

  readonly createUnselectableElement = createUnselectableElement;

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

  readonly createStylesheet = createStyleSheet;

  readonly StylesheetManager = StylesheetManager;
}