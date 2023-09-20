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

export interface ResolutionChain {
  type: 'chain',
  next: string,
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

  // This field is primarly used at the `GestureMatcher` level, rather than the
  // `PathMatcher` level.
  //
  // If there are multiple unresolved gestures, with no lock-in, the "potential gesture" with the
  // highest item priority is the authority re: the "current item".
  readonly itemPriority: number;

  // One or more "touchpath models" - how a touchpath matching this gesture would look, based on its
  // ordinal position.  (Same order as in the TrackedInput)
  readonly contacts: {
    model: ContactModel<Type>,
    endOnResolve?: boolean,
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