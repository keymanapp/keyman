import KeyboardStub from "./keyboardStub.js";
import { Keyboard } from "@keymanapp/keyboard-processor";

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

export default class StubAndKeyboardCache {
  private stubSetTable: Record<string, Record<string, KeyboardStub>> = {};
  private keyboardTable: Record<string, Keyboard | Promise<Keyboard>> = {};

  getKeyboardForStub(stub: KeyboardStub): Keyboard | Promise<Keyboard> {
    return this.getKeyboard(stub.KI);
  }

  addKeyboardForStub(stub: KeyboardStub, keyboard: Keyboard | Promise<Keyboard>) {
    this.addKeyboard(stub.KI, keyboard);
  }

  getKeyboard(keyboardID: string): Keyboard | Promise<Keyboard> {
    return this.keyboardTable[prefixed(keyboardID)];
  }

  addKeyboard(keyboardID: string, keyboard: Keyboard | Promise<Keyboard>) {
    keyboardID = prefixed(keyboardID);
    this.keyboardTable[keyboardID] = keyboard;

    if(keyboard instanceof Promise) {
      keyboard.then((kbd) => {
        this.keyboardTable[keyboardID] = kbd;
      });
    }
  }

  addStub(stub: KeyboardStub) {
    const keyboardID = prefixed(stub.KI);
    const stubTable = this.stubSetTable[keyboardID] = this.stubSetTable[keyboardID] ?? {};
    stubTable[stub.KLC] = stub;
  }

  findMatchingStub(stub: KeyboardStub) {
    return this.getStub(prefixed(stub.KI), stub.KLC);
  }

  getStub(keyboardID: string, languageID: string);
  getStub(keyboard: Keyboard, languageID?: string);
  getStub(arg0: string | Keyboard, arg1?: string) {
    let keyboardID: string;
    let languageID = arg1 || '---';

    if(arg0 instanceof Keyboard) {
      keyboardID = arg0.id;
    } else {
      keyboardID = arg0;
    }

    keyboardID = prefixed(keyboardID);

    const stubTable = this.stubSetTable[keyboardID] ?? {};

    return stubTable[languageID];
  }
}