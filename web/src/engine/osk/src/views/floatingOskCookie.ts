import { CookieSerializer } from 'keyman/engine/dom-utils';

export interface FloatingOSKCookie {
  visible: '0' | '1';
  userSet: '0' | '1';
  left: string;
  top: string;
  width?: string;
  height?: string;

  _version: string;
}

export class FloatingOSKCookieSerializer extends CookieSerializer<Required<FloatingOSKCookie>> {
  constructor() {
    super('KeymanWeb_OnScreenKeyboard');
  }

  load() {
    const cookie = super.load((value, key) => {
      switch(key) {
        case 'visible':
        case 'userSet':
          return Number.parseInt(value, 10);
        default:
          return value;
      }
    });

    if(!cookie['width']) {
      delete cookie['width'];  // in case of a '' entry.
    }
    if(!cookie['height']) {
      delete cookie['height']; // in case of a '' entry.
    }

    return cookie;
  }

  save(cookie: Required<FloatingOSKCookie>) {
    super.save(cookie);
  }
}