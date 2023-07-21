import { SimpleGestureSource } from "../../simpleGestureSource.js";
import { ContactModel, PointModelResolution } from "../specs/contactModel.js";
import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";

type FulfillmentCause = 'path' | 'timer' | 'item';

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
  private model: ContactModel<Type>;
  private source: SimpleGestureSource<Type>;

  private readonly publishedPromise: ManagedPromise<PathMatchResult>

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

  private finalize(result: boolean, cause: FulfillmentCause) {
    if(this.publishedPromise.isFulfilled) {
      return;
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
    this.publishedPromise.resolve(retVal);
    return retVal;
  }

  update(): PathUpdateResult {
    const model = this.model;
    const source = this.source;

    if(source.path.wasCancelled) {
      return this.finalize(false, 'path');
    }

    if(model.itemChangeAction && source.currentSample.item != source.initialSample.item) {
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
        return {
          type: 'reject',
          cause: 'path'
        };
      }

      return {
        type: 'continue'
      };
    }
  }
}