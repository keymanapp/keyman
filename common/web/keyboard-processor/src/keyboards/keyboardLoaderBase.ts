// TODO:  export from ../index.ts as well?

import Keyboard from "./keyboard.js";
import { KeyboardHarness } from "./keyboardHarness.js";

export default abstract class KeyboardLoaderBase {
  private _harness: KeyboardHarness;

  public get harness(): KeyboardHarness {
    return this._harness;
  }

  constructor(harness: KeyboardHarness) {
    this._harness = harness;
  }

  public loadKeyboardFromPath(uri: string): Promise<Keyboard> {
    this.harness.install();
    const promise = this.loadKeyboardInternal(uri);

    return promise;
  }

  protected abstract loadKeyboardInternal(uri: string): Promise<Keyboard>;
}