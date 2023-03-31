import type RealizedGesture from './realizedGesture.interface.js';
import { type KeyElement } from '../../keyElement.js';

/**
 * Used for evaluating potential gestures.  Classes adhering to this interface
 * should be instantiated whenever the (implied) state-machine allows a new
 * touch event to mark the start of a potential new gesture.
 *
 * For example, whenever a user touches a base key and there are no "realized"
 * (fully-completed, but as-of-yet unresolved) gestures, that state allows the
 * start of a potential new longpress event.
 *
 * The role of the `PendingGesture` is complete whenever all touch-events and
 * conditions necessary for a modeled gesture have been met.  As this point,
 * it should be `resolve`d, fulfilling its `promise`.  This results in a
 * `RealizedGesture` appropriate for the gesture type that is used to obtain
 * the final `KeyEvent` for the overall gesture sequence.
 *
 * For example, a "longpress" is considered resolved once the user has maintained
 * an active, stationary touch point on the same key for a sufficiently long
 * period without releasing it.
 * * Were it released earlier, that would result in selection of a base key.
 *
 * Alternatively, a "flick" might be considered resolved if:
 * * a user has rapidly moved a touch point in a consistent direction
 * * for a long enough distance
 * * and _then_ releases that touch point within a short timeframe.
 *
 * The pending gesture should only `resolve` to a realized gesture once
 * _all_ such conditions are met, confirming that this specific gesture,
 * and _only_ this specific gesture, could have resulted from the active
 * touch sequence.
 *
 * The `RealizedGesture` that results and is 'returned' via the Promise will
 * be handled by the `VisualKeyboard` class, which will retrieve and forward
 * any `KeyEvent` that results from the overall gesture input sequence.
 *
 * @see `RealizedGesture`
 */
export default interface PendingGesture {
  readonly baseKey: KeyElement;
  readonly promise?: Promise<RealizedGesture>;

  cancel(): void;
  resolve?(): void;
}