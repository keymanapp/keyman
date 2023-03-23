import Keyboard from "./keyboard.js";
import { KeyboardHarness } from "./keyboardHarness.js";
import KeyboardProperties from "./keyboardProperties.js";

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

  public loadKeyboardFromStub(stub: KeyboardProperties & { filename: string }) {
    this.harness.install();
    let promise = this.loadKeyboardInternal(stub.filename, stub.id);

    promise = promise.catch((err: Error) => {
      if(err == KeyboardLoaderBase.scriptErrorMessage(stub.filename)) {
        // Enhance the error message.
        err.message = KeyboardLoaderBase.scriptErrorMessage(stub);
      } else if(err == KeyboardLoaderBase.missingErrorMessage(stub.filename)) {
        // Same thing here.
        err.message = KeyboardLoaderBase.scriptErrorMessage(stub);
      }

      throw err;
    })

    return promise;
  }

  protected abstract loadKeyboardInternal(uri: string, id?: string): Promise<Keyboard>;

  protected static scriptErrorMessage(uri: string);
  protected static scriptErrorMessage(metadata: KeyboardProperties & { filename: string });
  protected static scriptErrorMessage(arg: string | (KeyboardProperties & { filename: string })) {
    if(typeof arg == "string") {
      const uri = arg;
      return `Error registering the keyboard script at ${uri}; it may contain an error.`
    } else {
      const stub = arg;
      return `Error registering the ${stub.name} keyboard for ${stub.langName}; keyboard script at ${stub.filename} may contain an error.`
    }
  }

  protected static missingErrorMessage(uri: string);
  protected static missingErrorMessage(metadata: KeyboardProperties & { filename: string });
  protected static missingErrorMessage(arg: string | (KeyboardProperties & { filename: string })) {
    if(typeof arg == "string") {
      const uri = arg;
      return `Cannot find the keyboard at ${uri}.`
    } else {
      const stub = arg;
      return `Cannot find the ${stub.name} keyboard for ${stub.langName} at ${stub.filename}.`;
    }
  }
}