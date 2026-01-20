import { KM_Core, KM_CORE_STATUS } from 'keyman/engine/core-adapter';
import { JSKeyboard } from "./jsKeyboard.js";
import { KMXKeyboard } from './kmxKeyboard.js';
import { KeyboardHarness } from "./keyboardHarness.js";
import { KeyboardProperties } from "./keyboardProperties.js";
import { KeyboardLoadErrorBuilder, StubBasedErrorBuilder, UriBasedErrorBuilder } from './keyboardLoadError.js';
import { Codes } from '../codes.js';

export enum NotifyEventCode {
  FocusEvent = 0,
  ShiftKey = Codes.keyCodes.K_SHIFT,
  ControlKey = Codes.keyCodes.K_CONTROL,
  AltKey = Codes.keyCodes.K_ALT,
};

export type KeyboardStub = KeyboardProperties & { filename: string };
export type Keyboard = JSKeyboard | KMXKeyboard;

export abstract class KeyboardLoaderBase {
  private _harness: KeyboardHarness;

  public get harness(): KeyboardHarness {
    return this._harness;
  }

  constructor(harness: KeyboardHarness) {
    this._harness = harness;
  }

  /**
   * Load a keyboard from a remote or local URI.
   *
   * @param uri  The URI of the keyboard to load.
   * @returns    A Promise that resolves to the loaded keyboard.
   */
  public loadKeyboardFromPath(uri: string): Promise<Keyboard> {
    this.harness.install();
    return this.loadKeyboardInternal(uri, new UriBasedErrorBuilder(uri));
  }

  /**
   * Load a keyboard from keyboard stub.
   *
   * @param stub  The stub of the keyboard to load.
   * @returns     A Promise that resolves to the loaded keyboard.
   */
  public async loadKeyboardFromStub(stub: KeyboardStub): Promise<Keyboard> {
    this.harness.install();
    return this.loadKeyboardInternal(stub.filename, new StubBasedErrorBuilder(stub));
  }

  private isKMXKeyboard(byteArray: Uint8Array): boolean {
    // Check if the first 4 bytes are 'KXTS' (0x4b, 0x58, 0x54, 0x53)
    return byteArray.length >= 4 && byteArray[0] === 0x4b &&
      byteArray[1] === 0x58 && byteArray[2] === 0x54 && byteArray[3] === 0x53;
  }

  private async loadKeyboardInternal(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard> {
    const byteArray = await this.loadKeyboardBlob(uri, errorBuilder);

    if (this.isKMXKeyboard(byteArray)) {
      // KMX or LDML (KMX+) keyboard
      const result = KM_Core.instance.keyboard_load_from_blob(uri, byteArray);
      if (result.status == KM_CORE_STATUS.OK) {
        return new KMXKeyboard(result.object);
      }
      return null;
    }

    let script: string;
    try {
      script = new TextDecoder('utf-8', { fatal: true }).decode(byteArray);
    } catch (e) {
      throw errorBuilder.invalidKeyboard(e);
    }

    // .js keyboard
    return await this.loadKeyboardFromScript(script, errorBuilder);
  }

  protected abstract loadKeyboardBlob(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Uint8Array>;

  protected abstract loadKeyboardFromScript(scriptSrc: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<JSKeyboard>;
}