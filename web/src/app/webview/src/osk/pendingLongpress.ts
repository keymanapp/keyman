
import {
  type KeyElement,
  PendingGesture,
  VisualKeyboard
} from 'keyman/engine/osk';

import { SubkeyDelegator } from "./subkeyDelegator.js";

/**
 * As control over the subkey display timer and the subkey popup are
 * both handled by the host app within the Android app, this class
 * serves mostly to communicate longpress state management from the
 * app to the VisualKeyboard.
 *
 * The `resolve()` function should be triggered by the host app
 * whenever it has recognized a completed longpress and has thus
 * begun displaying its subkey popup.  (Should these two events
 * ever become separated in time, the latter is the more critical
 * aspect.)
 */
export class PendingLongpress implements PendingGesture {
  private resolver: (delegator: SubkeyDelegator) => void;
  private readonly vkbd: VisualKeyboard;

  public readonly baseKey: KeyElement;
  public readonly promise: Promise<SubkeyDelegator>;

  constructor(vkbd: VisualKeyboard, e: KeyElement) {
    this.vkbd = vkbd;
    let _this = this;

    this.promise = new Promise<SubkeyDelegator>(function(resolve) {
      _this.resolver = resolve;
    });
    this.baseKey = e;
  }

  public resolve() {
    if(this.resolver) {
      this.resolver(new SubkeyDelegator(this.vkbd, this.baseKey));
    }
    this.resolver = null;
  }

  public cancel() {
    if(this.resolver) {
      this.resolver(null);
      this.resolver = null;
    }
  }
}