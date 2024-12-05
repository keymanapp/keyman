import { JSKeyboard } from "./keyboard.js";
import Codes from "../codes.js";
import { DeviceSpec } from '@keymanapp/web-utils';

/**
 * Defines members of the top-level `keyman` global object necessary to guarantee
 * successful loading of a keyboard.
 */
export interface KeyboardKeymanGlobal {
  // Omitting this will prevent debug-compiled keyboards from loading.
  readonly osk: MinimalCodesInterface;
}

/**
 * Defines any public API points used by debug-compiled keyboards for human-readable
 * nomenclature in rules within a Keyman keyboard's script for keyboard rules.
 *
 * Refer to TCompileKeymanWeb.JavaScript_SetupDebug.
 */
export interface MinimalCodesInterface {
  readonly modifierCodes: typeof Codes.modifierCodes;
  readonly keyCodes: typeof Codes.keyCodes;
  readonly modifierBitmasks: typeof Codes.modifierBitmasks;
  readonly stateBitmasks: typeof Codes.stateBitmasks;
}

export const MinimalKeymanGlobal: KeyboardKeymanGlobal = {
  // While this CERTAINLY isn't polymorphic with an actual OSK, it's "enough" to
  // facilitate loading of debug-compiled Keyman keyboards that rely on constants
  // defined within Codes at relevant legacy endpoints.
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
  activeDevice: DeviceSpec;


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
  public loadedKeyboard: JSKeyboard = null;

  /**
   * Keyman keyboards register themselves into the Keyman Engine for Web by directly
   * calling `KeymanWeb.KR` - the function provided here after "installation" of the harness.
   *
   * Note that the resulting Keyboard's internal rule scripting has its global object bound via
   * closure/capture; that global object must have a rule-compatible harness set in order to
   * facilitate keyboard-rule evaluation.  Note that the mechanism for keyboard loading in
   * Node (for headless unit tests) supplies a unique global per load if not supplied to the
   * constructor that is not accessible for manipulation after the load!
   *
   * If the supplied global's accessible keyboard harness does not supply keyboard rule API
   * functions, attempts to process keyboard rules will throw `Error`s instead.
   * @param scriptObject
   */
  public KR(scriptObject: any) {
    if(this.loadedKeyboard) {
      throw new Error("Unexpected state:  the most-recently loaded keyboard field was not properly reset.");
    }
    this.loadedKeyboard = new JSKeyboard(scriptObject);
  }

  // Is evaluated on script-load for some keyboards using variable stores.
  // Example:  sil_ipa - store(option_key)
  public KLOAD(kbdName: string, storeName: string, dfltValue: string) {
    return dfltValue;
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

  public uninstall() {
    if(this._jsGlobal.KeymanWeb == this) {
      delete this._jsGlobal.KeymanWeb;
    }

    if(this._jsGlobal.keyman == this.keymanGlobal) {
      delete this._jsGlobal.keyman;
    }
  }
}

