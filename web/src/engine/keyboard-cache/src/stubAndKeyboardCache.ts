import { Keyboard, KeyboardLoaderBase as KeyboardLoader } from "@keymanapp/keyboard-processor";
import EventEmitter from "eventemitter3";
import { type PathConfiguration } from "keyman/engine/configuration";

import KeyboardStub from "./keyboardStub.js";

const KEYBOARD_PREFIX = "Keyboard_";

function prefixed(text: string) {
  if(!text.startsWith(KEYBOARD_PREFIX)) {
    return KEYBOARD_PREFIX + text;
  } else {
    return text;
  }
}

export {prefixed as toPrefixedKeyboardId};

function withoutPrefix(text: string) {
  if(text.startsWith(KEYBOARD_PREFIX)) {
    return text.substring(KEYBOARD_PREFIX.length);
  } else {
    return text;
  }
}

export {withoutPrefix as toUnprefixedKeyboardId};

interface EventMap {
  /**
   * Indicates that the specified stub has just been registered within the cache.
   */
  stubAdded: (stub: KeyboardStub) => void;

  /**
   * Indicates that the specified Keyboard has just been added to the cache.
   */
  keyboardAdded: (keyboard: Keyboard) => void;
}

export default class StubAndKeyboardCache extends EventEmitter<EventMap> {
  private stubSetTable: Record<string, Record<string, KeyboardStub>> = {};
  private keyboardTable: Record<string, Keyboard | Promise<Keyboard>> = {};

  private readonly keyboardLoader: KeyboardLoader;

  constructor(keyboardLoader?: KeyboardLoader) {
    super();
    this.keyboardLoader = keyboardLoader;
  }

  getKeyboardForStub(stub: KeyboardStub): Keyboard {
    return this.getKeyboard(stub.KI);
  }

  getKeyboard(keyboardID: string): Keyboard {
    const entry = this.keyboardTable[prefixed(keyboardID)];
    return entry instanceof Keyboard ? entry : null;
  }

  addKeyboard(keyboard: Keyboard) {
    const keyboardID = prefixed(keyboard.id);
    this.keyboardTable[keyboardID] = keyboard;

    this.emit('keyboardAdded', keyboard);
  }

  fetchKeyboardForStub(stub: KeyboardStub) : Promise<Keyboard> {
    return this.fetchKeyboard(stub.KI);
  }

  fetchKeyboard(keyboardID: string): Promise<Keyboard> {
    if(!keyboardID) {
      throw new Error("Keyboard ID must be specified");
    }

    if(!this.keyboardLoader) {
      throw new Error("Cannot load keyboards; this cache was configured without a loader");
    }

    keyboardID = prefixed(keyboardID);

    const cachedEntry = this.keyboardTable[keyboardID];
    if(cachedEntry instanceof Keyboard) {
      return Promise.resolve(cachedEntry);
    } else if(cachedEntry instanceof Promise) {
      return cachedEntry;
    }

    const stub = this.getStub(keyboardID, null);
    if(!stub) {
      throw new Error(`No stub for ${withoutPrefix(keyboardID)} has been registered`);
    }

    if(!stub.filename) {
      throw new Error(`The registered stub for ${withoutPrefix(keyboardID)} lacks a path to the main keyboard file`);
    }

    const promise = this.keyboardLoader.loadKeyboardFromPath(stub.filename);
    this.keyboardTable[keyboardID] = promise;

    promise.then((kbd) => {
      this.addKeyboard(kbd);
    }).catch((err) => {
      delete this.keyboardTable[keyboardID];
      throw err;
    })

    return promise;
  }

  addStub(stub: KeyboardStub) {
    const keyboardID = prefixed(stub.KI);
    const stubTable = this.stubSetTable[keyboardID] = this.stubSetTable[keyboardID] ?? {};
    stubTable[stub.KLC] = stub;

    this.emit('stubAdded', stub);
  }

  findMatchingStub(stub: KeyboardStub) {
    return this.getStub(stub.KI, stub.KLC);
  }

  getStub(keyboardID: string, languageID: string): KeyboardStub;
  getStub(keyboard: Keyboard, languageID?: string): KeyboardStub;
  getStub(arg0: string | Keyboard, arg1?: string): KeyboardStub {
    let keyboardID: string;
    let languageID = arg1 || '---';

    if(arg0 instanceof Keyboard) {
      keyboardID = arg0.id;
    } else {
      keyboardID = arg0;
    }

    keyboardID = prefixed(keyboardID);

    const stubTable = this.stubSetTable[keyboardID] ?? {};

    if(languageID != '---') {
      return stubTable[languageID];
    } else {
      const keys = Object.keys(stubTable);
      if(keys.length == 0) {
        return null;
      } else {
        return stubTable[keys[0]];
      }
    };
  }

  /**
   * Removes all metadata (stubs) associated with a specific keyboard from the cache, optionally
   * removing the cached keyboard as well.
   * @param keyboard Either the keyboard ID or `Keyboard` instance
   * @param purge If `true`, will also purge the `Keyboard` instance itself from the cache.
   *              If `false`, only forgets the metadata (stubs).
   */
  forgetKeyboard(keyboard: string | Keyboard, purge: boolean = false) {
    let id: string = (keyboard instanceof Keyboard) ? keyboard.id : keyboard;

    if(this.stubSetTable[id]) {
      delete this.stubSetTable[id];
    }

    if(purge && this.keyboardTable[id]) {
      delete this.keyboardTable[id];
    }
  }
}