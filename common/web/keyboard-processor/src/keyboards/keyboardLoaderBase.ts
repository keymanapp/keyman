import Keyboard from "./keyboard.js";
import { KeyboardHarness } from "./keyboardHarness.js";
import KeyboardProperties from "./keyboardProperties.js";

type KeyboardStub = KeyboardProperties & { filename: string };

/**
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/cause
 *
 * While TS doesn't yet recognize it as a property of error, recent versions of JS define it.
 * So, we should obviously conform to that spec, even if older versions of JS won't expect it
 * to exist.
 */
type ErrorWithCause = Error & { cause?: Error };

export interface KeyboardLoadErrorBuilder {
  scriptError(err?: Error);
  missingError(err: Error);
}

const buildAndFinalizeError = (msg: string, err?: Error): ErrorWithCause => {
  const builtError: ErrorWithCause = new Error(msg);
  builtError.cause = err;

  return builtError;
}

class UriBasedErrorBuilder implements KeyboardLoadErrorBuilder {
  readonly uri: string;

  constructor(uri: string) {
    this.uri = uri;
  }

  missingError(err: Error) {
    const msg = `Cannot find the keyboard at ${this.uri}.`;
    return buildAndFinalizeError(msg, err);
  }

  scriptError(err: Error) {
    const msg = `Error registering the keyboard script at ${this.uri}; it may contain an error.`;
    return buildAndFinalizeError(msg, err);
  }
}

class StubBasedErrorBuilder implements KeyboardLoadErrorBuilder {
  readonly stub: KeyboardStub;

  constructor(stub: KeyboardStub) {
    this.stub = stub;
  }

  missingError(err: Error) {
    const stub = this.stub;
    const msg = `Cannot find the ${stub.name} keyboard for ${stub.langName} at ${stub.filename}.`;
    return buildAndFinalizeError(msg, err);
  }

  scriptError(err: Error) {
    const stub = this.stub;
    const msg = `Error registering the ${stub.name} keyboard for ${stub.langName}; keyboard script at ${stub.filename} may contain an error.`;
    return buildAndFinalizeError(msg, err);
  }
}

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
    const promise = this.loadKeyboardInternal(uri, new UriBasedErrorBuilder(uri));

    return promise;
  }

  public loadKeyboardFromStub(stub: KeyboardStub) {
    this.harness.install();
    let promise = this.loadKeyboardInternal(stub.filename, new StubBasedErrorBuilder(stub), stub.id);

    return promise;
  }

  protected abstract loadKeyboardInternal(
    uri: string,
    errorBuilder: KeyboardLoadErrorBuilder,
    id?: string
  ): Promise<Keyboard>;
}