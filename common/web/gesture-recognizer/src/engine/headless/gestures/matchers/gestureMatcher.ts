import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";

import { GestureModel, GestureResolution, GestureResolutionSpec, RejectionDefault, RejectionReplace, ResolutionItemSpec } from "../specs/gestureModel.js";

import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";
import { FulfillmentCause, PathMatcher } from "./pathMatcher.js";

export interface PredecessorMatch<Type> {
  readonly sources: GestureSource<Type>[];
  readonly allSourceIds: string[];
  readonly primaryPath: GestureSource<Type>;
  readonly result: MatchResult<Type>;
  readonly model?: GestureModel<Type>;
  readonly baseItem: Type;
}

export interface MatchResult<Type> {
  readonly matched: boolean,
  readonly action: GestureResolution<Type>
}

export interface MatchResultSpec {
  readonly matched: boolean,
  readonly action: GestureResolutionSpec
}

export class GestureMatcher<Type> implements PredecessorMatch<Type> {
  private sustainTimerPromise?: TimeoutPromise;
  public readonly model: GestureModel<Type>;

  private readonly pathMatchers: PathMatcher<Type>[];

  public get sources(): GestureSource<Type>[] {
    return this.pathMatchers.map((pathMatch, index) => {
      if(this.model.contacts[index].resetOnResolve) {
        return undefined;
      } else {
        return pathMatch.source;
      }
    }).filter((entry) => !!entry);
  }

  private readonly predecessor?: PredecessorMatch<Type>;

  private readonly publishedPromise: ManagedPromise<MatchResult<Type>>; // unsure on the actual typing at the moment.
  private _result: MatchResult<Type>;

  public get promise() {
    return this.publishedPromise.corePromise;
  }

  constructor(model: GestureModel<Type>, sourceObj: GestureSource<Type> | PredecessorMatch<Type>) {
    /* c8 ignore next 5 */
    if(!model || !sourceObj) {
      throw new Error("Construction of GestureMatcher requires a gesture-model spec and a source for related contact points.");
    } else if(!model.sustainTimer && !sourceObj) {
      throw new Error("If the provided gesture-model spec lacks a sustain timer, there must be an active contact point.");
    }

    // We condition on ComplexGestureSource since some unit tests mock the other type without
    // instantiating the actual type.
    const predecessor = sourceObj instanceof GestureSource<Type> ? null : sourceObj;
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
      return entry.isPathComplete ? null : entry;
    }).reduce((cleansed, entry) => {
      return entry ? cleansed.concat(entry) : cleansed;
    }, []);

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
          this.addContactInternal(contact);
          continue;
        case 'partial':
          preserveBaseItem = true;
          // Intentional fall-through
        case 'chop':
          contact = srcContact.constructSubview(true, preserveBaseItem);
          this.addContactInternal(contact);
          break;
      }
    }
  }

  private finalize(matched: boolean, cause: FulfillmentCause) {
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
        // Some gesture types may wish to restart with a new base item if they fail due to
        // it changing during its lifetime or due to characteristics of the contact-point's
        // path.
        if(this.model.rejectionActions?.[cause]) {
          action = this.model.rejectionActions[cause];
          action.item = 'none';
        }

        // Rejection for other reasons, or if no special action is defined for item rejection:
        action = action || {
          type: 'none',
          item: 'none'
        };
      }

      for(let i = 0; i < this.pathMatchers.length; i++) {
        const matcher = this.pathMatchers[i];
        const contactSpec = this.model.contacts[i];

        // If the path already terminated, no need to evaluate further for this contact point.
        if(matcher.source.isPathComplete) {
          continue;
        }

        if(matched && contactSpec.endOnResolve) {
          matcher.source.terminate(false);
        } else if(!matched && contactSpec.endOnReject) {
          // Ending due to gesture-rejection effectively means to cancel the path,
          // so signal exactly that.
          matcher.source.terminate(true);
        }
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
    let highestPriority = Number.MIN_VALUE;
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

    this.addContactInternal(simpleSource.constructSubview(false, true));
  }

  public get result() {
    return this._result;
  }

  private addContactInternal(simpleSource: GestureSourceSubview<Type>) {
    // The number of already-active contacts tracked for this gesture
    const existingContacts = this.pathMatchers.length;

    const contactSpec = this.model.contacts[existingContacts];
    const contactModel = new PathMatcher(contactSpec.model, simpleSource);
    // Add it early, as we need it to be accessible for reference via .primaryPath stuff below.
    this.pathMatchers.push(contactModel);

    let baseItem: Type = null;
    // If there were no existing contacts but a predecessor exists and a sustain timer
    // has been specified, it needs special base-item handling.
    if(!existingContacts && this.predecessor && this.model.sustainTimer) {
      const baseItemMode = this.model.sustainTimer.baseItem ?? 'result';

      switch(baseItemMode) {
        case 'none':
          baseItem = null;
          break;
        case 'base':
          baseItem = this.predecessor.primaryPath.baseItem;
          break;
        case 'result':
          baseItem = this.predecessor.result.action.item;
          break;
      }
    } else {
      // just use the highest-priority item source's base item and call it a day.
      // There's no need to refer to some previously-existing source for comparison.
      baseItem = this.primaryPath.baseItem;
    }

    if(contactSpec.model.allowsInitialState) {
      const initialStateCheck = contactSpec.model.allowsInitialState(simpleSource.currentSample, this.primaryPath?.currentSample, baseItem);

      if(!initialStateCheck) {
        this.finalize(false, 'path');
      }
    }

    contactModel.promise.then((resolution) => {
      this.finalize(resolution.type == 'resolve', resolution.cause);
    });
  }

  update() {
    this.pathMatchers.forEach((matcher) => matcher.update());
  }
}