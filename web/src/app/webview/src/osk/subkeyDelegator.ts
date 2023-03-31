import { type ActiveKey, type KeyEvent } from '@keymanapp/keyboard-processor';

import {
  type InputEventCoordinate,
  type KeyElement,
  type OSKKeySpec,
  RealizedGesture,
  VisualKeyboard
} from 'keyman/engine/osk';

/**
 * As the subkey popup view is handled by the host app when in embedded mode
 * within our Android app, this class represents the fact that KMW has
 * "delegated" subkey UI and selection to the host app.  Hence, "Delegator",
 * rather than "Popup".
 *
 * The `resolve` method should be triggered, in some fashion, by the host app
 * whenever the user has completed their longpress, potentially selecting
 * a subkey.
 *
 * This class will also track the ongoing touch event in case the base key is
 * reselected, which _is_ managed by this class, not the host app.
 */
export class SubkeyDelegator implements RealizedGesture {
  private resolver: (keyEvent: KeyEvent) => void;
  private readonly vkbd: VisualKeyboard;

  public readonly baseKey: KeyElement;
  public readonly promise: Promise<KeyEvent>;

  private movedFromBaseKey: boolean = false;
  private baseKeySelected: boolean = false;

  constructor(vkbd: VisualKeyboard, e: KeyElement) {
    this.vkbd = vkbd;

    let _this = this;
    this.promise = new Promise<KeyEvent>(function(resolve) {
      _this.resolver = resolve;
    });

    this.baseKey = e;
  }

  /**
   * Resolves the ongoing longpress -> subkey gesture, fulfilling this
   * `SubkeyDelegator`'s `promise` of a `KeyEvent`.
   *
   * If no subkey is selected but the original base key is, `resolve(null)`
   * will return a key event corresponding to the base key.
   *
   * @param keyCoreID   {string}  The 'core ID' (id + modifier layer) of
   *                              a selected subkey.  May be `null`.
   */
  public resolve(keyCoreID: string) {
    if(this.resolver) {
      let keyEvent: KeyEvent = null;

      if(keyCoreID == null && this.baseKeySelected) {
        // Handle selection of base key underneath the subkey array.
        keyEvent = this.vkbd.keyEventFromSpec(this.baseKey.key.spec as ActiveKey, null);
        this.baseKey.key.highlight(false);
      } else if(keyCoreID != null) {
        // This is set with the base key of our current subkey elsewhere within the engine.
        let baseKey: OSKKeySpec = this.baseKey.key.spec;
        let selectedKey: OSKKeySpec;

        if(baseKey.coreID == keyCoreID) {
          selectedKey = baseKey;
        } else {
          // ... yeah, there are some funky type shenanigans between the two.
          // OSKKeySpec is the OSK's... reinterpretation of the ActiveKey type.
          selectedKey = (baseKey as ActiveKey).getSubkey(keyCoreID) as OSKKeySpec;
        }

        if(!selectedKey) {
          // While we can't complete successfully, the subkey operation is done; we
          // should still signal that and update related gesture state management.
          this.resolver(null);
          console.error("Could not find subkey '" + keyCoreID + "' under base key '" + baseKey.coreID + "'!");
          return;
        }

        keyEvent = this.vkbd.keyEventFromSpec(selectedKey as ActiveKey, null);
        keyEvent.vkCode=keyEvent.Lcode;
      } // else /* if(keyCoreID == null) */ keyEvent = null; // As initialized at the top.

      this.resolver(keyEvent);
    }
    this.resolver = null;
  }

  public isVisible(): boolean {
    return true;
  }

  public clear() {
    // no-op; it's fully controlled on the app side.
  }

  /**
   * Allows this class to detect if the user may have changed their mind and
   * re-selected the base key.
   * @param touch
   */
  updateTouch(input: InputEventCoordinate) {
    this.baseKeySelected = this.baseKey.key.isUnderTouch(input);

    // Prevent highlighting & selection before the touch has moved from the base key.
    if(this.movedFromBaseKey) {
      this.baseKey.key.highlight(this.baseKeySelected);
    } else {
      this.movedFromBaseKey = !this.baseKeySelected;
    }
  }
}