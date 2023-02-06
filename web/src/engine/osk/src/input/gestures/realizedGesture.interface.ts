import InputEventCoordinate from '../inputEventCoordinate.js';
import { type KeyElement } from '../../keyElement.js';
import { type KeyEvent } from '@keymanapp/keyboard-processor';

/**
 * Implementations of this interface allow individual types of gestures to
 * specify any additional user interaction and functionality (which may
 * include UI elements) appropriate for obtaining a key event that may be
 * produced by the modeled gesture type.  These should only be instantiated
 * once the associated `PendingLongpress` is no longer 'pending' - once it
 * has become clear that the input touch-event sequence could only correspond
 * to the modeled gesture.
 *
 * For example, when a longpress gesture completes - and hence, the user has
 * kept their finger stationary on the same key for a long enough period -
 * we display a popup view presenting subkeys corresponding to the gesture's
 * underlying element.  This popup view accepts touch input and completes only
 * upon release of the ongoing touch sequence.
 *
 * Gestures are events that occur over intervals of time, and since some of them
 * will require time and user interaction after becoming 'realized', these cases
 * will be inherently async.  The simplest way to model this is with `Promise`s.
 *
 * If appropriate for the modeled gesture type, an implementation may supply an
 * instantly-resolving `Promise`.  This may be appropriate for modeling "flick"
 * or "swipe" gestures in the future, which may require no additional input once
 * such a gesture is fully realized.
 */
export default interface RealizedGesture {
  readonly baseKey: KeyElement;
  readonly promise: Promise<KeyEvent>;

  clear(): void;
  isVisible(): boolean;
  updateTouch(input: InputEventCoordinate): void;
}