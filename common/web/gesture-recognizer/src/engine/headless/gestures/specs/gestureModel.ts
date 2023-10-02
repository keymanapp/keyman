import { MatchResult } from "../matchers/gestureMatcher.js";
import { GestureSequence } from "../matchers/gestureSequence.js";
import { FulfillmentCause } from "../matchers/pathMatcher.js";
import { ContactModel } from "./contactModel.js";

// So doc-comments can be inherited together.
export interface ResolutionItemSpec {
  item?: 'base' | 'current' | 'none',
}

export interface ResolutionItem<Type> {
  item: Type
}

/**
 * Indicates that the matched gesture is but a component (or stage) of a
 * multi-part gesture; there may be one or more follow-up components
 * that will follow.
 */
export interface ResolutionChain {
  type: 'chain',
  // For consideration:  string | string[];  // But we don't need the latter part for 17.0 gesture support.
  /**
   * The gesture ID for the next gesture component in sequence.
   *
   * E.g. longpress => subkey-select; that is, 'subkey-select' would be next after the 'longpress' model
   * matches.
   */
  next: string,

  /**
   * When specified, gesture-component selection for new GestureSources will use the specified
   * set of models instead of the current default set for new sources.
   *
   * Example 1:  longpresses, when transitioning to subkey-select mode, do not allow new incoming
   * gestures during their lifetime.  They should either cancel or block new gestures until
   * subkey-selection is complete.
   *
   * Example 2:  modipress operations should prevent secondary modipresses from occurring during
   * their lifetime.
   *
   * Followup gesture-models must also specify the alternate model set in order to maintain it during
   * transition between components.  Leaving it `undefined` in a followup will fully cancel the
   * alternate gesture-component selection mode and any gestures activated during the alternate
   * selection mode (unless `sustainIfNested` is `true` for the processing gesture model).
   *
   * Changing to a different ID will do the likewise, then reactivate the alternate gesture-selection
   * mode with the newly-specified gesture-model set target.
   */
  selectionMode?: string
}

export interface ResolutionComplete {
  type: 'complete'
}

export interface RejectionDefault {
  type: 'none'
}

/**
 * Only permitted when rejecting a gesture match; certain models may specify a replacement
 * or reset under certain conditions.
 */
export interface RejectionReplace {
  type: 'replace',

  // For consideration:  string | string[];  // But we don't need the latter part for 17.0 gesture support.
                                             // Is trickier here for 'replace' than for 'chain'.
  /**
   * The ID of a gesture model to start matching as a replacement for the gesture-model that failed to match.
   */
  replace: string
}

// If there is a 'gesture stack' associated with the gesture chain, it's auto-popped
// upon completion of the chain.  So, either this resolution type or a final,
// non-chainable rejection will 'pop' to undo any existing prior 'push' resolutions
// in the chain.  As such, there is no need for a {type: 'pop'} variant.

type ResolutionStruct = ResolutionChain | ResolutionComplete;

export type GestureResolutionSpec   = ResolutionStruct & ResolutionItemSpec;
export type GestureResolution<Type> = (ResolutionStruct | RejectionDefault | RejectionReplace) & ResolutionItem<Type>;

export interface GestureModel<Type> {
  // Gestures may want to say "build gesture of type `id`" for a followup-gesture.
  readonly id: string;

  // This field is primarly used at the `GestureMatcher` level, rather than the
  // `PathMatcher` level.
  //
  // Higher = better.  Only takes effect if multiple gesture types could resolve on the same
  // ComplexGestureSource for the same update.
  readonly resolutionPriority: number;

  // One or more "touchpath models" - how a touchpath matching this gesture would look, based on its
  // ordinal position.  (Same order as in the TrackedInput)
  readonly contacts: {
    model: ContactModel<Type>,
    /**
     * Indicates that the corresponding GestureSource should not be considered part of the
     * Gesture sequence being matched, acting more as a separate gesture that 'triggers' a state
     * change in the current gesture being processed.
     */
    resetOnResolve?: boolean,
    /**
     * Indicates that the corresponding GestureSource should be terminated whenever this GestureModel
     * is successfully matched.
     */
    endOnResolve?: boolean,
    /**
     * Indicates that the corresponding GestureSource should be terminated whenever this GestureModel
     * _fails_ to match.
     */
    endOnReject?: boolean
  }[];

  // if this is defined, the gesture can't resolve while the spec'd Promise is active.
  // Even if `expectedResult` is negative.
  readonly sustainTimer?: {
    duration: number,
    expectedResult: boolean,

    // Determines which base item from an ancestor should be used for any initial-state checks for
    // paths that come in during the sustain.
    baseItem?: 'base' | 'result' | 'none'
  }

  readonly resolutionAction: GestureResolutionSpec;

  readonly rejectionActions?: Partial<Record<FulfillmentCause, RejectionReplace>>;
  // If there is a 'gesture stack' associated with the gesture chain, it's auto-popped
  // upon completion of the chain.  Optional-chaining can sustain the chain while the
  // potential child gesture is still a possibility.

  // If we're locked-in on the gesture being matched and its detection occurs under the influence
  // of another gesture, should that "another gesture" complete, this flag specifies if the
  // locked-in "subgesture" should be maintained or auto-cancelled as a consequence.
  //
  // Default:  cancelled.
  readonly sustainWhenNested?: boolean;

  // TODO:  allow function for correlating multitouch paths (like for caret-pannning)
  // But that's something we'll likely defer past 17.0.
  // Probably:  takes both paths' stat-objects.  (Fortunately, the stats object holds
  // 'start' and 'end' sample references - so we don't need to add them as additional
  // parameters.)
}