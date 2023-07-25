import { CumulativePathStats } from "../../cumulativePathStats.js";
import { SimpleGestureSource } from "../../simpleGestureSource.js";
import { ContactModel } from "../specs/contactModel.js";
import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";

export type FulfillmentCause = 'path' | 'timer' | 'item';

export interface PathMatchResolution {
  type: 'resolve',
  cause: FulfillmentCause
}

export interface PathMatchRejection {
  type: 'reject'
  cause: FulfillmentCause
}

export interface PathNotFulfilled {
  type: 'continue'
}

type PathMatchResult = PathMatchRejection | PathMatchResolution;
type PathUpdateResult = PathMatchResult | PathNotFulfilled;

export class PathMatcher<Type> {
  private timerPromise?: TimeoutPromise;
  public readonly model: ContactModel<Type>;

  // During execution, source.path is fine... but once this matcher's role is done,
  // `source` will continue to receive edits and may even change the instance
  // underlying the `path` field.
  public readonly source: SimpleGestureSource<Type>;

  private _finalStats: CumulativePathStats<Type>;
  private _baseItem: Type;

  private readonly publishedPromise: ManagedPromise<PathMatchResult>
  private _result: PathMatchResult;

  public get promise() {
    return this.publishedPromise.corePromise;
  }

  constructor(model: ContactModel<Type>, source: SimpleGestureSource<Type>) {
    /* c8 ignore next 3 */
    if(!model || !source) {
      throw new Error("A gesture-path source and contact-path model must be specified.");
    }

    this.model = model;
    this.publishedPromise = new ManagedPromise<PathMatchResult>();
    this.source = source;

    if(model.timer) {
      this.timerPromise = new TimeoutPromise(model.timer.duration);

      this.publishedPromise.then(() => {
        this.timerPromise.resolve(false);
        // but make sure that simultaneous path resolution continues even if the timer's is mismatched.
      });

      this.timerPromise.then((result) => {
        this.finalize(result == model.timer.expectedResult, 'timer');
      });
    }
  }

  private finalize(result: boolean, cause: FulfillmentCause): PathMatchResult {
    if(this.publishedPromise.isFulfilled) {
      return this._result;
    }

    const model = this.model;
    let retVal: PathMatchResult;
    if(result) {
      retVal = {
        type: model.pathResolutionAction,
        cause: cause
      };
    } else {
      retVal = {
        type: 'reject',
        cause: cause
      };
    }
    this.publishedPromise.resolve(retVal)
    this._result = retVal;

    /*
     * Lock in the current instance of path, before further processing on the SimpleGestureSource
     * performs any 'chop'-style operations.
     *
     * No need to deep copy; the only mutable part is the "followingSample" field, which relates to
     * any path continuation by later stages of the ongoing gesture path.  That's fine, as it's
     * not the part we actually need to preserve in a fixed state.
     *
     * While we _could_, in theory, preserve the overall path... we don't need the samples -
     * just the stats will do.
     */
    this._finalStats = this.source.path.stats;
    this._baseItem = this.source.baseItem;

    return retVal;
  }

  get finalStats() {
    // Note:  contains `initialSample` and `lastSample`, the endpoints of the path segment that was matched.
    // The latter can be used to find the matcher's final `currentItem` value if/as desired.
    return this._finalStats;
  }

  get baseItem() {
    if(this.publishedPromise.isFulfilled) {
      return this._baseItem;
    } else {
      return this.source.baseItem;
    }
  }

  get lastItem() {
    if(this.publishedPromise.isFulfilled) {
      return this.finalStats.lastSample.item;
    } else {
      return this.source.currentSample.item;
    }
  }

  update(): PathUpdateResult {
    const model = this.model;
    const source = this.source;

    if(source.path.wasCancelled) {
      return this.finalize(false, 'path');
    }

    if(model.itemChangeAction && source.currentSample.item != source.baseItem) {
      const result = model.itemChangeAction == 'resolve';

      return this.finalize(result, 'item');
    } else {
      // Note:  is current path, not 'full path'.
      const result = model.pathModel.evaluate(source.path) || 'continue';

      if(result != 'continue') {
        return this.finalize(result == 'resolve', 'path');
      } else if(source.path.isComplete) {
        // If the PathModel said to 'continue' but the path is done, we default
        // to rejecting the model; there will be no more changes, after all.
        return this.finalize(false, 'path');
      }

      return {
        type: 'continue'
      };
    }
  }
}