import { PointModel } from './pointModel.js';

// Leave this as an interface; it's easier for external stuff to specify that way.
export interface InputModel<ItemType> {  // Only generic b/c TouchpointModel<ItemType>.
  // Gestures may want to say "build gesture of type `id`" for a followup-gesture.
  readonly id: string;

  // Higher = better.
  readonly resolutionPriority: number;

  // If there are multiple unresolved gestures, with no lock-in, the "potential gesture" with the
  // highest item priority is the authority re: the "current item".
  readonly itemPriority: number;

  // One or more "touchpath models" - how a touchpath matching this gesture would look, based on its
  // ordinal position.  (Same order as in the TrackedInput)
  readonly touchpathModels: PointModel<ItemType>[]; // May be able to drop the generic bit!

  // if this is defined, the gesture can't resolve while the spec'd Promise is active.
  // Even if `expectedResult` is negative.
  readonly awaitModel?: {
    duration: number,
    expectedResult: boolean
  }

  // what if the gesture can preserve items from linked predecessors, rather than the predecessor haphazardly forwarding them?
  // Makes more sense?
  //
  // Default if undefined:
  // - initial:  the ancestor 'initial' (likely, the touchpath's true initial item)
  //
  // If multiple paths are inherited, but only a string is set, the specified mode will be used for all paths.
  // If an array, but more paths exist than have an entry, the last will be used as default.
  readonly initialItemSource: ('initial' | 'current')[] | 'initial' | 'current';

  // 'reject':  multitaps must not have a lingering prior path.  If a simple touch was triggered due to a second touchpoint,
  // 'reject' will fail the multitouch because the second touchpoint's existence will continue and attempt inheritance
  readonly pathInheritance?: 'none' | 'reset' | 'full' | 'reject';

  readonly onResolution: {
    // At least one must be set.
    nextGesture?: string,
    item?: 'initial' | 'current' | 'none',
    terminatePaths: number[]; // the ordinal indices of currently-tracked paths to force-terminate.
    lockedIn?: boolean; // default - false
                        // (a possible second tap of a multitap should not be 'locked in', but 'third' on should)
  }

  // The next item is more speculative than the rest of the defined properties.
  // May be useful for longpress-resets when the key shifts?
  readonly onRejection: {
    // At least one must be set.
    nextGesture?: {
      id: string,
    },
    item?: 'initial' | 'current' | 'none',
    terminatePaths: number[]; // the ordinal indices of currently-tracked paths to force-terminate.
  }
}

// something to match state of TrackedInput with GestureModel above.

/*
 * TODO for specific gesture support:
 *
 * Upon recognition of a gesture that continues with another, the consumer should be allowed to stack-push
 * an entirely new GestureRecognizerConfiguration.  In particular, for longpress subkey menu support.
 * - Note:  might complicate TrackedPath tracking management & the input engines?
 *   - probably:  have a config 'stack'... and so long as the touch doesn't go outside of the top-most stack's config,
 *     we'd be fine.
 * - I'm not 100% on what form this would take for the theoretical spun-off WebView based Android app subkey menu,
 *   where the subkeys would be displayed in a separate WebView from the one that triggered the subkey menu.  But
 *   "we can cross that bridge when we come to it".
 *   - key thing:  we can set _precedent_ for delegating control/completion of the gesture to something else if/as
 *     desired.
 *
 * May also be wise to allow alternating with a Promise (b/c Android embedded mode).
 * - Perhaps the Promise itself says "i'll take over from here; resume when the gesture resolves".
 * - Could be a separate instance (Android WebView) or just delegation to an entirely different controller
 *   (the current Android-app delegation to Java-based gesture handling)
 */

/****** BRAINSTORMING NOTES (helped me process & develop the interfaces, etc spec'd in this source folder) ******/

  /*
   * First touches:
   * - on path-termination, auto-completes
   * - on sufficient pathing, auto-completes base gesture, makes a second
   *   - longpress (shortcut) => subkey menu
   *   - waaaaaait.  "makes a second" - so it can pre-build the model-match for the next stage, maybe?
   * - after sufficient time + still-matching path, auto-completes
   *   - longpress => subkey menu
   *   - must track timeout on the gesture model, since we don't have the subsegmentation model yet
   *     - which would better facilitate hold length
   *     - unless we put something related on the _stats_ object... which isn't the best fit
   *     - may need ability to reset it / reset the longpress-hold (when roaming touch is allowed)
   * - after sufficient time, auto-cancels
   *   - flick (?)
   * - on path-termination, does NOT (fully) auto-complete, though may make secondary gesture
   *   - multitap => continued-multitap
   * - may allow 'resets'?
   *   - roaming touch => longpress-timer restart?
   */

  /*
   * Second touches:
   * - accepts a second; specifies legal paths (caret panning, possibly modifierpress)
   *   - !!under specific conditions!!         (caret panning:  must start within threshold of #1, near #1)
   *   - is assimilated by the gesture; no new Input
   * - rejects a second & auto-cancels         (new touchpoint during a subkey menu)
   *   - may or may not need new Input - is kind of undefined
   * - rejects a second while autocompleting   (simple touch)
   *   - is not assimilated by the gesture, does allow new Input / gesture from the trigger
   *   - does reject a multitap
   * - just... ignores a second                (modifierpress?)
   *
   * - multitap:  can complete a first, have an empty slot, no second touch
   *   => continued-multitap:  can accept a (new) first with constraints, no second touch
   *     - or "that's all"
   *   => continued-multitap (again)
   *     - or "that's all"
   */