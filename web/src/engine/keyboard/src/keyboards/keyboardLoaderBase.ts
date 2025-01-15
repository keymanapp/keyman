import { JSKeyboard } from "./jsKeyboard.js";
import { KeyboardHarness } from "./keyboardHarness.js";
import KeyboardProperties from "./keyboardProperties.js";
import { KeyboardLoadErrorBuilder, StubBasedErrorBuilder, UriBasedErrorBuilder } from './keyboardLoadError.js';

export type KeyboardStub = KeyboardProperties & { filename: string };

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
  public loadKeyboardFromPath(uri: string): Promise<JSKeyboard> {
    this.harness.install();
    return this.loadKeyboardInternal(uri, new UriBasedErrorBuilder(uri));
  }

  /**
   * Load a keyboard from keyboard stub.
   *
   * @param stub  The stub of the keyboard to load.
   * @returns     A Promise that resolves to the loaded keyboard.
   */
  public async loadKeyboardFromStub(stub: KeyboardStub): Promise<JSKeyboard> {
    this.harness.install();
    return this.loadKeyboardInternal(stub.filename, new StubBasedErrorBuilder(stub));
  }

  private async loadKeyboardInternal(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<JSKeyboard> {
    const byteArray = await this.loadKeyboardBlob(uri, errorBuilder);

    if (byteArray.slice(0, 4) == Uint8Array.from([0x4b, 0x58, 0x54, 0x53])) { // 'KXTS'
      // KMX or LDML (KMX+) keyboard
      console.error("KMX keyboard loading is not yet implemented!");
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