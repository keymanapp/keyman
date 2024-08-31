import Keyboard from "./keyboard.js";
import { KeyboardHarness } from "./keyboardHarness.js";
import KeyboardProperties from "./keyboardProperties.js";
import { KeyboardLoadErrorBuilder, StubBasedErrorBuilder, UriBasedErrorBuilder } from './keyboardLoadError.js';

export type KeyboardStub = KeyboardProperties & { filename: string };

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
    return this.loadKeyboardInternal(uri, new UriBasedErrorBuilder(uri));
  }

  public loadKeyboardFromStub(stub: KeyboardStub) {
    this.harness.install();
    return this.loadKeyboardInternal(stub.filename, new StubBasedErrorBuilder(stub));
  }

  private async loadKeyboardInternal(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard> {
    const blob = await this.loadKeyboardBlob(uri);

    const script = await blob.text();
    if (script.startsWith('KXTS', 0)) {
      // KMX or LDML (KMX+) keyboard
      console.error("KMX keyboard loading is not yet implemented!");
      return null;
    }

    // .js keyboard
    return await this.loadKeyboardFromScript(script, errorBuilder);
  }

  protected abstract loadKeyboardBlob(uri: string): Promise<Blob>;

  protected abstract loadKeyboardFromScript(scriptSrc: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard>;
}