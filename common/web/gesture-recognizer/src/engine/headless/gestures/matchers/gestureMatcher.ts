import { ComplexGestureSource } from "../../complexGestureSource.js";
import { SimpleGestureSource, SimpleGestureSourceSubview } from "../../simpleGestureSource.js";

import { GestureModel, GestureResolution, GestureResolutionSpec, RejectionDefault, ResolutionItemSpec } from "../specs/gestureModel.js";

import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";
import { FulfillmentCause, PathMatcher } from "./pathMatcher.js";

//
// TODO:  Next up - let's unit-test this bad boy!
//

export interface MatchResult<Type> {
  matched: boolean,
  action: GestureResolution<Type>
}

export interface MatchResultSpec {
  matched: boolean,
  action: GestureResolutionSpec
}

export class GestureMatcher<Type> {
  private sustainTimerPromise?: TimeoutPromise;
  public readonly model: GestureModel<Type>;

  public readonly pathMatchers: PathMatcher<Type>[];
  private predecessor?: GestureMatcher<Type>;

  private readonly publishedPromise: ManagedPromise<MatchResult<Type>>; // unsure on the actual typing at the moment.
  private _result: MatchResult<Type>;

  private baseSource: ComplexGestureSource<Type>;

  public get promise() {
    return this.publishedPromise.corePromise;
  }

  constructor(model: GestureModel<Type>, sourceObj: ComplexGestureSource<Type> | GestureMatcher<Type>) {
    /* c8 ignore next 5 */
    if(!model || !sourceObj) {
      throw new Error("Construction of GestureMatcher requires a gesture-model spec and a source for related contact points.");
    } else if(!model.sustainTimer && sourceObj instanceof ComplexGestureSource && sourceObj.touchpoints.length == 0) {
      throw new Error("If the provided gesture-model spec lacks a sustain timer, there must be an active contact point.");
    }

    // We condition on ComplexGestureSource since some unit tests mock the other type without
    // instantiating the actual type.
    const predecessor = sourceObj instanceof ComplexGestureSource<Type> ? null : sourceObj;
    const source = predecessor ? null : (sourceObj as ComplexGestureSource<Type>);

    this.baseSource = predecessor?.baseSource || source;

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

    let sourceTouchpoints: SimpleGestureSource<Type>[];
    if(source) {
      sourceTouchpoints = source.touchpoints;
    } else {
      sourceTouchpoints = predecessor.pathMatchers.map((matcher) => matcher.source);
    }

    let offset = 0;
    for(let touchpointIndex = 0; touchpointIndex < sourceTouchpoints.length; touchpointIndex++) {
      const srcContact = sourceTouchpoints[touchpointIndex];

      // If a touchpoint's path is already complete, ignore it when modeling a new gesture.
      if(srcContact.path.isComplete) {
        offset++;
        continue;
      }

      if(srcContact instanceof SimpleGestureSourceSubview) {
        srcContact.disconnect();  // prevent further updates from mangling tracked path info.
      }
      let i = touchpointIndex - offset;

      const contactSpec = model.contacts[i];
      /* c8 ignore next 3 */
      if(!contactSpec) {
        throw new Error(`No contact model for inherited path: gesture "${model.id}', entry ${i}`);
      }
      const inheritancePattern = contactSpec?.model.pathInheritance ?? 'chop';

      let preserveBaseItem: boolean = false;

      let contact: SimpleGestureSource<Type>;
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
      let action: GestureResolutionSpec | (RejectionDefault & ResolutionItemSpec);
      if(matched) {
        // Easy peasy - resolutions only need & have the one defined action type.
        action = this.model.resolutionAction;
      } else {
        // Some gesture types may wish to restart with a new base item if they fail due to
        // it changing during its lifetime or due to characteristics of the contact-point's
        // path.
        if(this.model.rejectionActions && this.model.rejectionActions[cause]) {
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
          resolutionItem = this.comparisonStandard.baseItem;
          break;
        case 'current':
          resolutionItem = this.comparisonStandard.currentSample.item;
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
  private get comparisonStandard(): SimpleGestureSource<Type> {
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
      return this.predecessor.comparisonStandard;
    }

    return bestMatcher.source;
  }

  mayAddContact(): boolean {
    return this.pathMatchers.length < this.model.contacts.length;
  }

  // for new incoming SimpleGestureSource
  addContact(simpleSource: SimpleGestureSource<Type>) {
    const existingContacts = this.pathMatchers.length;
    /* c8 ignore next 3 */
    if(!this.mayAddContact()) {
      throw new Error(`The specified gesture model does not support more than ${existingContacts} contact points.`);
    }

    // The number of already-active contacts tracked for this gesture
    const contactSpec = this.model.contacts[existingContacts];

    let baseItem: Type = null;
    if(existingContacts) {
      // just use the highest-priority item source's base item and call it a day.
      baseItem = this.comparisonStandard.baseItem;
    } else if(this.predecessor && this.model.sustainTimer) {
      const baseItemMode = this.model.sustainTimer.baseItem ?? 'result';

      switch(baseItemMode) {
        case 'none':
          baseItem = null;
          break;
        case 'base':
          baseItem = this.predecessor.comparisonStandard.baseItem;
          break;
        case 'result':
          baseItem = this.predecessor._result.action.item;
          break;
      }
    }

    if(contactSpec.model.allowsInitialState) {
      const initialStateCheck = contactSpec.model.allowsInitialState(simpleSource.currentSample, this.comparisonStandard.currentSample, baseItem);

      if(!initialStateCheck) {
        this.finalize(false, 'path');
      }
    }

    this.addContactInternal(simpleSource.constructSubview(false, true));
  }

  private addContactInternal(simpleSource: SimpleGestureSource<Type>) {
    const existingContacts = this.pathMatchers.length;

    // The number of already-active contacts tracked for this gesture
    const contactSpec = this.model.contacts[existingContacts];
    const contactModel = new PathMatcher(contactSpec.model, simpleSource);
    contactModel.promise.then((resolution) => {
      this.finalize(resolution.type == 'resolve', resolution.cause);
    });

    this.pathMatchers.push(contactModel);
  }

  update() {
    this.pathMatchers.forEach((matcher) => matcher.update());
  }
}