import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";

import { GestureModel, GestureResolution, GestureResolutionSpec, RejectionDefault, RejectionReplace, ResolutionItemSpec } from "../specs/gestureModel.js";

import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";
import { FulfillmentCause, PathMatcher } from "./pathMatcher.js";
import { CumulativePathStats } from "../../cumulativePathStats.js";
import { processSampleClientCoords } from "../../../inputEventEngine.js";

/**
 * This interface specifies the minimal data necessary for setting up gesture-selection
 * among a set of gesture models that will conceptually follow from the most
 * recently-matched gesture-model.  The most standard implementation of this is the
 * `GestureMatcher` class.
 *
 * Up until very recently, KeymanWeb would delegate certain gestures to be handled by
 * host apps when it was in an embedded state.  While that pattern has been dropped,
 * the abstraction gained from reaching compatibility with it is useful.  Either way,
 * for such scenarios, as long as fulfilled gestures can be linked to an implementation
 * of this interface, they can be integrated into the gesture-sequence staging system -
 * even if not matched directly by the recognizer itself.
 */
export interface PredecessorMatch<Type, StateToken> {
  readonly sources: GestureSource<Type>[];
  readonly allSourceIds: string[];
  readonly primaryPath: GestureSource<Type, StateToken>;
  readonly result: MatchResult<Type>;
  readonly model?: GestureModel<Type, any>;
  readonly baseItem: Type;
  readonly predecessor?: PredecessorMatch<Type, StateToken>;
}

export interface MatchResult<Type> {
  readonly matched: boolean,
  readonly action: GestureResolution<Type>
}

export interface MatchResultSpec {
  readonly matched: boolean,
  readonly action: GestureResolutionSpec
}

export class GestureMatcher<Type, StateToken = any> implements PredecessorMatch<Type, StateToken> {
  private sustainTimerPromise?: TimeoutPromise;
  public readonly model: GestureModel<Type, StateToken>;

  private readonly pathMatchers: PathMatcher<Type>[];

  public get sources(): GestureSource<Type>[] {
    return this.pathMatchers.map((pathMatch, index) => {
      if(this.model.contacts[index].resetOnInstantFulfill) {
        return undefined;
      } else {
        return pathMatch.source;
      }
    }).filter((entry) => !!entry);
  }

  private _isCancelled: boolean = false;

  readonly predecessor?: PredecessorMatch<Type, StateToken>;

  private readonly publishedPromise: ManagedPromise<MatchResult<Type>>; // unsure on the actual typing at the moment.
  private _result: MatchResult<Type>;

  public get promise() {
    return this.publishedPromise.corePromise;
  }

  constructor(
    model: GestureModel<Type, StateToken>,
    sourceObj: GestureSource<Type> | PredecessorMatch<Type, StateToken>
  ) {
    /* c8 ignore next 5 */
    if(!model || !sourceObj) {
      throw new Error("Construction of GestureMatcher requires a gesture-model spec and a source for related contact points.");
    } else if(!model.sustainTimer && !sourceObj) {
      throw new Error("If the provided gesture-model spec lacks a sustain timer, there must be an active contact point.");
    }

    // We condition on ComplexGestureSource since some unit tests mock the other type without
    // instantiating the actual type.
    const predecessor = sourceObj instanceof GestureSource ? null : sourceObj;
    const source = predecessor ? null : (sourceObj as GestureSource<Type>);

    this.predecessor = predecessor;
    this.publishedPromise = new ManagedPromise();

    this.model = model;
    if(model.sustainTimer) {
      this.sustainTimerPromise = new TimeoutPromise(model.sustainTimer.duration);
      this.sustainTimerPromise.then((elapsed) => {
        const shouldResolve = model.sustainTimer.expectedResult == elapsed;
        this.finalize(shouldResolve, 'timer');
      });
    }

    this.pathMatchers = [];

    const unfilteredSourceTouchpoints: GestureSource<Type>[] = source
      ? [ source ]
      : predecessor.sources;

    const sourceTouchpoints = unfilteredSourceTouchpoints.map((entry) => {
      if(source && entry == source) {
        // Due to internal delays that can occur when an incoming tap triggers
        // completion of a previously-existing gesture but is not included in it
        // (`resetOnInstantFulfill` mechanics), it is technically possible for a very
        // quick tap to be 'complete' by the time we start trying to match
        // against it on some devices.  We should still try in such cases.
        return source;
      } else {
        return entry.isPathComplete ? null : entry;
      }
    }).reduce((cleansed, entry) => {
      return entry ? cleansed.concat(entry) : cleansed;
    }, [] as GestureSource<Type>[]);

    if(model.sustainTimer && sourceTouchpoints.length > 0) {
      // If a sustain timer is set, it's because we expect to have NO gesture-source _initially_.
      // If we actually have one, that's cause for rejection.
      //
      this.finalize(false, 'path');
      return;
    } else if(!model.sustainTimer && sourceTouchpoints.length == 0) {
      // If no sustain timer is set, we don't start against the specified set; that'll happen
      // once there's an actual source to support the modeled gesture.
      this.finalize(false, 'path');
    }

    for(let touchpointIndex = 0; touchpointIndex < sourceTouchpoints.length; touchpointIndex++) {
      const srcContact = sourceTouchpoints[touchpointIndex];

      if(srcContact instanceof GestureSourceSubview) {
        srcContact.disconnect();  // prevent further updates from mangling tracked path info.
      }

      // No need to filter out already-matched contact points, and doing so is more trouble
      // than its worth.

      const contactSpec = model.contacts[touchpointIndex];
      /* c8 ignore next 3 */
      if(!contactSpec) {
        throw new Error(`No contact model for inherited path: gesture "${model.id}', entry ${touchpointIndex}`);
      }
      const inheritancePattern = contactSpec?.model.pathInheritance ?? 'chop';

      let preserveBaseItem: boolean = false;

      let contact: GestureSourceSubview<Type>;
      switch(inheritancePattern) {
        case 'reject':
          this.finalize(false, 'path');
          return;
        case 'full':
          contact = srcContact.constructSubview(false, true);
          this.addContactInternal(contact, srcContact.path.stats, true);
          continue;
        case 'partial':
          preserveBaseItem = true;
          // Intentional fall-through
        case 'chop':
          contact = srcContact.constructSubview(true, preserveBaseItem);
          this.addContactInternal(contact, srcContact.path.stats, true);
          break;
      }
    }
  }

  public cancel() {
    this._isCancelled = true;
    if(!this._result) {
      this.finalize(false, 'cancelled');
    }
  }

  public get isCancelled(): boolean {
    return this._isCancelled;
  }

  private finalize(matched: boolean, cause: FulfillmentCause): MatchResult<Type> {
    if(this.publishedPromise.isFulfilled) {
      return this._result;
    }

    try {
      // Determine the correct action-spec that should result from the finalization.
      let action: GestureResolutionSpec | ((RejectionDefault | RejectionReplace) & ResolutionItemSpec);
      if(matched) {
        // Easy peasy - resolutions only need & have the one defined action type.
        action = this.model.resolutionAction;
      } else {
        /*
          Some gesture types may wish to restart with a new base item if they fail due to
          it changing during its lifetime or due to characteristics of the contact-point's
          path.

          If a gesture model match is outright-cancelled, matcher restarts should be completely
          blocked.  One notable reason:  if a model-match is _immediately_ cancelled due to
          initial conditions, reattempting it can cause an infinite (async) loop!
        */
        if(cause != 'cancelled' && this.model.rejectionActions?.[cause]) {
          action = this.model.rejectionActions[cause];
          action.item = 'none';
        }

        // Rejection for other reasons, or if no special action is defined for item rejection:
        action = action || {
          type: 'none',
          item: 'none'
        };
      }

      // Determine the item source for the item to be reported for this gesture, if any.
      let resolutionItem: Type;
      const itemSource = action.item ?? 'current';
      switch(itemSource) {
        case 'none':
          resolutionItem = null;
          break;
        case 'base':
          resolutionItem = this.primaryPath.baseItem;
          break;
        case 'current':
          resolutionItem = this.primaryPath.currentSample.item;
          break;
      }

      // Do actual resolution now that we can convert the spec into a proper resolution object.
      let resolveObj: MatchResult<Type> = {
        matched: matched,
        action: {
          ...action,
          item: resolutionItem
        }
      };

      this.publishedPromise.resolve(resolveObj);

      this._result = resolveObj;
      return resolveObj;
      /* c8 ignore next 3 */
    } catch(err) {
      this.publishedPromise.reject(err);
      return {
        matched: false,
        action: {
          type: 'none',
          item: null
        }
      }
    }
  }

  /**
   * Applies any source-finalization specified by the model based on whether or not it was matched.
   * It is invalid to call this method before model evaluation is complete.
   *
   * Additionally, this should only be applied for "selected" gesture models - those that "win"
   * and are accepted as part of a GestureSequence.
   */
  public finalizeSources() {
    if(!this._result) {
      throw Error("Invalid state for source-finalization - the matcher's evaluation of the gesture model is not yet complete");
    }

    const matched = this._result.matched;
    for(let i = 0; i < this.pathMatchers.length; i++) {
      const matcher = this.pathMatchers[i];
      const contactSpec = this.model.contacts[i];

      /* Future TODO:
       * This should probably include "committing" the state token and items used by the subview,
       * should they differ from the base source's original values.
       *
       * That said, this is only a 'polish' task, as we aren't actually relying on the base source
       * once we've started identifying gestures.  It'll likely only matter if external users
       * desire to utilize the recognizer.
       */

      // If the path already terminated, no need to evaluate further for this contact point.
      if(matcher.source.isPathComplete) {
        continue;
      }

      if(matched && contactSpec.endOnResolve) {
        matcher.source.terminate(false);
      } else if(!matched && contactSpec.endOnReject) {
        matcher.source.terminate(false);
      }
    }
  }

  /**
   * Determines the active path-matcher best suited to serve as the "primary" path for the gesture.
   *
   * This is needed for the following logic roles:
   * - If accepting a new contact point, `allowsInitialState` needs an existing path's sample for
   *   comparison.
   * - When resolving, the "primary" path determines what item (if any) is generated for the gesture.
   *
   * If no matcher is active, but the currently-evaluating gesture has a direct ancestor, the best
   * matcher from the predecessor may be used instead.
   */
  public get primaryPath(): GestureSource<Type> {
    let bestMatcher: PathMatcher<Type>;
    let highestPriority = Number.NEGATIVE_INFINITY;
    for(let matcher of this.pathMatchers) {
      if(matcher.model.itemPriority > highestPriority) {
        highestPriority = matcher.model.itemPriority;
        bestMatcher = matcher;
      }
    }

    // Example case:  multitap, with the current stage of the chain having zero active touchpaths...
    // but a previous 'link' having had a valid touchpath.
    //
    // Here, the best answer is to use the 'comparisonPath' from the prior link; it'll contain
    // the path-samples we'd intuitively expect to use for comparison, after all.
    if(!bestMatcher && this.predecessor) {
      return this.predecessor.primaryPath;
    }

    return bestMatcher?.source;
  }

  public get baseItem(): Type {
    return this.primaryPath.baseItem;
  }

  public get currentItem(): Type {
    return this.primaryPath.currentSample.item;
  }

  /*
   * Gets the GestureSource identifier corresponding to the gesture being matched
   * and all predecessor stages.  All are relevant for resolving gesture-selection;
   * predecessor IDs become relevant for gesture stages that start without an
   * active GestureSource.  (One that's not already finished its path)
   *
   * In theory, just one predecessor previous should be fine, rather than
   * 'all'... but that'd take a little extra work.
   */
  public get allSourceIds(): string[] {
    // Do not include any to-be-reset (thus, excluded) sources here.
    let currentIds = this.sources.map((entry) => entry.identifier);
    const predecessorIds = this.predecessor ? this.predecessor.allSourceIds : [];

    // Each ID should only be listed once, regardless of source.
    currentIds = currentIds.filter((entry) => predecessorIds.indexOf(entry) == -1);

    return currentIds.concat(predecessorIds);
  }

  mayAddContact(): boolean {
    return this.pathMatchers.length < this.model.contacts.length;
  }

  // for new incoming GestureSource
  addContact(simpleSource: GestureSource<Type>) {
    const existingContacts = this.pathMatchers.length;
    /* c8 ignore next 3 */
    if(!this.mayAddContact()) {
      throw new Error(`The specified gesture model does not support more than ${existingContacts} contact points.`);
    }

    this.addContactInternal(simpleSource.constructSubview(false, true), null);
  }

  public get result() {
    return this._result;
  }

  private addContactInternal(simpleSource: GestureSourceSubview<Type>, basePathStats: CumulativePathStats<Type>, whileInitializing?: boolean) {
    // The number of already-active contacts tracked for this gesture
    const existingContacts = this.pathMatchers.length;

    const contactSpec = this.model.contacts[existingContacts];
    const contactModel = new PathMatcher(contactSpec.model, simpleSource, new CumulativePathStats(basePathStats));
    // Add it early, as we need it to be accessible for reference via .primaryPath stuff below.
    this.pathMatchers.push(contactModel);

    let ancestorSource: GestureSource<Type> = null;
    let baseItem: Type = null;
    // If there were no existing contacts but a predecessor exists and a sustain timer
    // has been specified, it needs special base-item handling.
    if(!existingContacts && this.predecessor && this.model.sustainTimer) {
      ancestorSource = this.predecessor.primaryPath;
      const baseItemMode = this.model.sustainTimer.baseItem ?? 'result';
      let baseStateToken: StateToken;

      switch(baseItemMode) {
        case 'none':
          baseItem = null;
          break;
        case 'base':
          baseItem = this.predecessor.primaryPath.baseItem;
          baseStateToken = this.predecessor.primaryPath.stateToken;
          break;
        case 'result':
          baseItem = this.predecessor.result.action.item;
          baseStateToken = this.predecessor.primaryPath.currentSample.stateToken;
          break;
      }

      // Under 'sustain timer' mode, the concept is that the first new source is the
      // continuation and successor to `predecessor.primaryPath`. Its base `item`
      // should reflect this.
      simpleSource.baseItem = baseItem ?? simpleSource.baseItem;
      simpleSource.stateToken = baseStateToken;
      simpleSource.currentSample.stateToken = baseStateToken;

      // May be missing during unit tests.
      if(simpleSource.currentRecognizerConfig) {
        simpleSource.currentSample.item = simpleSource.currentRecognizerConfig.itemIdentifier(
          simpleSource.currentSample,
          null
        );
      }
    } else {
      // just use the highest-priority item source's base item and call it a day.
      // There's no need to refer to some previously-existing source for comparison.
      baseItem = this.primaryPath.baseItem;
      ancestorSource = this.primaryPath;
    }


    if(contactSpec.model.baseCoordReplacer) {
      const originalStats = simpleSource.path.stats;
      const replacementSampleBase = contactSpec.model.baseCoordReplacer(originalStats, baseItem);

      if(replacementSampleBase) {
        // 1. Complete the sample
        const replacementSample = {
          ...processSampleClientCoords(
            simpleSource.currentRecognizerConfig,
            replacementSampleBase.clientX,
            replacementSampleBase.clientY
          ),
          t: (replacementSampleBase.t || replacementSampleBase.t === 0) ? replacementSampleBase.t : originalStats.initialSample.t,
          item: baseItem,
          stateToken: simpleSource.stateToken
        };

        // 2. Replace it within the source's path-stats.
        simpleSource.path.replaceInitialSample(replacementSample);
      }
    }

    // Check that initial "item" and "state" properties are legal for this type of gesture.
    if(contactSpec.model.allowsInitialState) {
      const initialStateCheck = contactSpec.model.allowsInitialState(
        simpleSource.currentSample,
        ancestorSource.currentSample,
        baseItem,
        ancestorSource.stateToken
      );

      if(!initialStateCheck) {
        // The initial state check failed, and we should not permanently establish a
        // pathMatcher for a source that failed to meet initial conditions.
        this.pathMatchers.pop();

        /*
          To prevent any further retries for the model (via rejectionActions), we list the
          cause as 'cancelled'.  'Cancelled' match attempts will never be retried, and we
          wish to prevent an infinite (async) loop from retrying something we know will
          auto-cancel.  (That loop would automatically end upon a different model's match
          or upon all possible models failing to match at the same time, but it's still
          far from ideal.)

          The rejection-action mechanism in MatcherSelector's `replacer` method (refer to
          https://github.com/keymanapp/keyman/blob/be867604e4b2650a60e69dc6bbe0b6115315efff/common/web/gesture-recognizer/src/engine/headless/gestures/matchers/matcherSelector.ts#L559-L575)
          already blocks paths that are rejected synchronously by this method.  Use of
          'cancelled' is thus not necessary for avoiding the loop-scenario, but it does
          add an extra layer of protection.  Also, it's more explicit about the fact that
          we _are_ permanently cancelling any and all future attempts to match against
          it in the future for the affected `GestureSource`(s).

          If we weren't using 'cancelled', 'item' would correspond best with a rejection
          here, as the decision is made due to a validation check against the initial item.
        */
        this.finalize(false, 'cancelled');

        /*
         * There's no need to process the gesture-model any further... and the
         * invalid state may correspond to assumptions in the path-model that
         * will be invalidated if we continue.
         */
        return;
      }
    }

    /*
      Now that we've done the initial-state check, we can check for instantly-matching and
      instantly-rejecting path models... particularly from from causes other than initial-item
      and state, such as rejection due to an extra touch.

      KMW example:  longpresses cancel when a new touch comes in during the longpress timer;
      they should never become valid again for that base touch.
    */
    const result = contactModel.update();
    if(result?.type == 'reject') {
      /*
        Refer to the earlier comment in this method re: use of 'cancelled'; we
        need to prevent any and all further attempts to match against this model
        We'd instantly reject it anyway due to its rejected initial state.
        Failing to do so can cause an infinite async loop.

        If we weren't using 'cancelled', 'path' would correspond best with a
        rejection here, as the decision is made due to the GestureSource's
        current path being rejected by one of the `PathModel`s comprising the
        `GestureModel`.

        If the model's already been initialized, it's possible that a _new_
        incoming touch needs special handling.  We'll allow one reset.  In the
        case that it would try to restart itself, the restarted model will
        instantly fail and thus cancel.
      */
      this.finalize(false, whileInitializing ? 'cancelled' : 'path');
      return;
    }

    // Standard path:  trigger either resolution or rejection when the contact model signals either.
    contactModel.promise.then((resolution) => {
      this.finalize(resolution.type == 'resolve', resolution.cause);
    });
  }

  update() {
    this.pathMatchers.forEach((matcher) => {
      try {
        matcher.update();
      } catch(err) {
        console.error(err);
        this.finalize(false, 'cancelled');
      }
    });
  }
}