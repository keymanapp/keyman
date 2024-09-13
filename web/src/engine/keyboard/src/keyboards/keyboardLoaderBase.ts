import Keyboard from "./keyboard.js";
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
  public async loadKeyboardFromPath(uri: string): Promise<Keyboard> {
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

  private async loadKeyboardInternal(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard> {
    const blob = await this.loadKeyboardBlob(uri, errorBuilder);

    let script: string;
    try {
      script = await blob.text();
    } catch (e) {
      throw errorBuilder.invalidKeyboard(e);
    }

    if (script.startsWith('KXTS', 0)) {
      // KMX or LDML (KMX+) keyboard
      console.error("KMX keyboard loading is not yet implemented!");
      return null;
    }

    // .js keyboard
    return await this.loadKeyboardFromScript(script, errorBuilder);
  }

  protected abstract loadKeyboardBlob(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Blob>;

  protected abstract loadKeyboardFromScript(scriptSrc: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard>;
}