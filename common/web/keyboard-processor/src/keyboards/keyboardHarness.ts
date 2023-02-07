import Keyboard from "./keyboard.js";
import Codes from "../text/codes.js";

/**
 * Defines members of the top-level `keyman` global object necessary to guarantee
 * successful loading of a keyboard.
 */
export interface KeyboardKeymanGlobal {
  // Omitting this will prevent debug-compiled keyboards from loading.
  osk: MinimalCodesInterface;
}

/**
 * Defines any public API points used by debug-compiled keyboards for human-readable
 * nomenclature in rules within a Keyman keyboard's script for keyboard rules.
 *
 * Refer to C:\keymanapp\keyman\developer\src\tike\compile\CompileKeymanWeb.pas,
 * TCompileKeymanWeb.JavaScript_SetupDebug.
 */
export interface MinimalCodesInterface {
  modifierCodes: typeof Codes.modifierCodes;
  keyCodes: typeof Codes.keyCodes;
}

export const MinimalKeymanGlobal: KeyboardKeymanGlobal = {
  osk: Codes
}

/**
 * Defines the minimum interface needed to ensure successful loading of Keyman
 * keyboards of _any_ type.
 *
 * This interface is minimal; note that critical functions needed for the evaluation
 * of keyboard rules are not included.
 */
export class KeyboardHarness {
  public readonly _jsGlobal: any;
  public readonly keymanGlobal: KeyboardKeymanGlobal;

  /**
   * Constructs and configures a harness for receiving dynamically-loaded Keyman keyboards.
   *
   * @param _jsGlobal The object that the keyboard script will perceive as the top-level global
   *                  with the name `KeymanWeb`.
   * - In the DOM, as a member of `window` (which may be that of an iframe)
   * - In a WebWorker, as a member of `self`
   * - In Node, as a member of `global` (or of a context object supplied to `vm`)
   * @param keymanGlobal An object to stand-in as the `keyman` global variable within _jsGlobal.
   */
  public constructor(_jsGlobal: any, keymanGlobal: KeyboardKeymanGlobal) {
    this._jsGlobal = _jsGlobal;
    this.keymanGlobal = keymanGlobal;

    this.install();
  }

  /**
   * This field serves as the receptacle for a successfully-loaded Keyboard.
   */
  public activeKeyboard: Keyboard;

  /**
   * Keyman keyboards register themselves into the Keyman Engine for Web by directly
   * calling `KeymanWeb.KR` - the function provided here after "installation" of the harness.
   *
   * Note that the resulting Keyboard's internal rule scripting will be bound (via
   * closure/capture) to this specific instance of the harness.  Loading against a harness
   * that does not supply keyboard rule API functions will result in a Keyboard that cannot
   * evaluate rules - it will throw `Error`s instead.
   * @param scriptObject
   */
  public KR(scriptObject: any) {
    this.activeKeyboard = new Keyboard(scriptObject);
  }

  /**
   * Installs this harness instance into the object that the keyboard script will perceive
   * as the top-level global with the name `KeymanWeb`.
   * - In the DOM, as a member of `window` (which may be that of an iframe)
   * - In a WebWorker, as a member of `self`
   * - In Node, as a member of `global` (or of a context object supplied to `vm`)
   *
   * In doing so, the following properties will have endpoints within the global:
   * - `KeymanWeb` - this harness instance
   * - `keyman` - the `minKeymanGlobal` constructor argument.
   */
  public install() {
    this._jsGlobal.KeymanWeb = this;
    this._jsGlobal.keyman = this.keymanGlobal;
  }
}

