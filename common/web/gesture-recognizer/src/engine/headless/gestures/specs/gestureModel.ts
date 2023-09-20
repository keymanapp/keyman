import { FulfillmentCause } from "../matchers/pathMatcher.js";
import { ContactModel } from "./contactModel.js";

// So doc-comments can be inherited together.
export interface ResolutionItemSpec {
  item?: 'base' | 'current' | 'none',
}

export interface ResolutionItem<Type> {
  item: Type
}

export interface ResolutionSetChange {
  type: 'setchange',
  // 'push' - for having NEW gestures (while still active) default to a different gesture set than the default
  //   - should probably rename (to avoid confusion with pushed configs)
  //   - modipress:  the gesture itself is the same; it just enables an alt state for OTHER gestures
  //   - an engine 'listener' for push signals should be useful to ensure new gestures use the alt config.
  //   - but... how about the current sequence's behaviors in response?  (the modipress particulars w release?)
  //   - waaaaaait.  what if this state could listen to gestures started during the state?
  //     - so... internal state management?
  //     - if push-state selector has no active / locked sequences - detect by checking for ref equality with selector?
  //       - as in, the way to check for such a condition.
  //       - so, push-state stuff ought use its own selector.  Which IS true, anyway - it may have a diff base set-id
  //         than the standard default - that's one of two possible reasons WHY the state would exist.
  //     - but... "sustaining" if the original, pushing sequence collapses?  How does that get handled / modeled properly
  //       in relation to any active sequences?
  //       - perhaps that's it?  "sustaining" - like the sustain timer supporting multitap?  But now, not from a timer.
  //       - flag for "sustain if has active subgesture"?
  //         - ... blend with sustainTimer?  to sustain: { timer: {}, subgesture: boolean }?
  //         - subgesturesDeferFinalization: boolean
  //   - ... wait a sec.  During a push, the gesture itself may still continue!!!
  allowedSet: string,
  next: string
}

export interface ResolutionChain {
  type: 'chain',  // TODO:  "lock"?
  next: string
}

// is not "locked-in"
export interface OptionalChain {
  type: 'optional-chain',  // TODO:  a plain 'chain'?
  allowNext: string
}

export interface ResolutionComplete {
  type: 'complete'
}

export interface RejectionDefault {
  type: 'none'
}


// If there is a 'gesture stack' associated with the gesture chain, it's auto-popped
// upon completion of the chain.  So, either this resolution type or a final,
// non-chainable rejection will 'pop' to undo any existing prior 'push' resolutions
// in the chain.  As such, there is no need for a {type: 'pop'} variant.

type ResolutionStruct = ResolutionSetChange | ResolutionChain | OptionalChain | ResolutionComplete;

export type GestureResolutionSpec   = ResolutionStruct & ResolutionItemSpec;
export type GestureResolution<Type> = (ResolutionStruct | RejectionDefault) & ResolutionItem<Type>;

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

  readonly rejectionActions?: Partial<Record<FulfillmentCause, Omit<OptionalChain, 'item'>>>;
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