import { CookieSerializer } from 'keyman/engine/dom-utils';

export interface FloatingOSKCookie {
  /**
   * Notes whether or not the OSK was hidden at the end of the previous session.
   */
  visible: 0 | 1;

  /**
   * Notes whether or not the OSK was pinned (located by the user) at the end
   * of the previous session.
   */
  userSet: 0 | 1;

  /**
   * Denotes the left-position of the OSK at the end of the previous session if pinned.
   * Defaults to -1 if the value was undefined.
   */
  left: number;

  /**
   * Denotes the left-position of the OSK at the end of the previous session if pinned.
   * Defaults to -1 if the value was undefined.
   */
  top: number;

  /**
   * The previously-set OSK width.
   */
  width?: number;

  /**
   * The previously-set OSK height.
   */
  height?: number;

  /**
   * The version of KeymanWeb active when this cookie was generated.
   */
  _version: string;
}

export class FloatingOSKCookieSerializer extends CookieSerializer<Required<FloatingOSKCookie>> {
  constructor() {
    super('KeymanWeb_OnScreenKeyboard');
  }

  loadWithDefaults(defaults: Required<FloatingOSKCookie>) {
    return {...defaults, ...this.load()};
  }

  load() {
    const cookie = super.load((value, key) => {
      switch(key) {
        case 'version':
          return value;
        default:
          return Number.parseInt(value, 10);
      }
    });

    if(!cookie.width) {
      delete cookie.width;  // in case of a '' entry.
    }
    if(!cookie.height) {
      delete cookie.height; // in case of a '' entry.
    }

    return cookie;
  }

  save(cookie: Required<FloatingOSKCookie>) {
    super.save(cookie);
  }
}