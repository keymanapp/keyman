// import { type KeyElement } from '../../../keyElement.js';
// import VisualKeyboard from '../../../visualKeyboard.js';
// import PendingGesture from '../pendingGesture.interface.js';
// import SubkeyPopup from './subkeyPopup.js';

// /**
//  * (Conceptually) represents a finite-state-machine that determines
//  * whether or not a series of touch events corresponds to a longpress
//  * touch input.  The `resolve` method may be used to trigger the
//  * subkey menu early, as with the upward quick-display shortcut.
//  *
//  * This is the default implementation of longpress behavior for KMW.
//  * Alterate implementations are modeled through the `embedded`
//  * namespace's equivalent, which is designed to facilitate custom
//  * modeling for such gestures.
//  *
//  * Once the conditions to recognize a longpress gesture have been
//  * fulfilled, this class's `promise` will resolve with a `SubkeyPopup`
//  * matching the gesture's 'base' key, which itself provides a
//  * `promise` field that will resolve to a `KeyEvent` once the touch
//  * sequence is completed.
//  */
// export default class PendingLongpress implements PendingGesture {
//   public readonly baseKey: KeyElement;
//   public readonly promise: Promise<SubkeyPopup>;

//   public readonly subkeyUI: SubkeyPopup;

//   private readonly vkbd: VisualKeyboard;
//   private resolver: (subkeyPopup: SubkeyPopup) => void;

//   private timerId: number;
//   private popupDelay: number = 500;

//   constructor(vkbd: VisualKeyboard, baseKey: KeyElement) {
//     this.vkbd = vkbd;
//     this.baseKey = baseKey;

//     let _this = this;
//     this.promise = new Promise<SubkeyPopup>(function(resolve, reject) {
//       _this.resolver = resolve;
//       // After the timeout, it's no longer deferred; it's being fulfilled.
//       // Even if the actual subkey itself is still async.
//       _this.timerId = window.setTimeout(_this.resolve.bind(_this), _this.popupDelay);
//     });
//   }

//   public cancel() {
//     if(this.timerId) {
//       window.clearTimeout(this.timerId);
//       this.timerId = null;
//     }

//     if(this.resolver) {
//       this.resolver(null);
//       this.resolver = null;
//     }
//   }

//   public resolve() {
//     // User has flicked up to get to the longpress, before
//     // the timeout has expired. We need to cancel the timeout.
//     // See #5950
//     if(this.timerId) {
//       window.clearTimeout(this.timerId);
//       this.timerId = null;
//     }

//     if(this.resolver) {
//       this.resolver(new SubkeyPopup(this.vkbd, this.baseKey));
//     }
//   }
// }
