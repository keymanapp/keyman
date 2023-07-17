import { SimpleGestureSource } from "../../simpleGestureSource.js";
import { ContactModel, PointModelResolution } from "../specs/contactModel.js";
import { ManagedPromise, TimeoutPromise } from "@keymanapp/web-utils";

export class PathMatcher<Type> {
  private timerPromise?: TimeoutPromise;
  private model: ContactModel;
  private source: SimpleGestureSource<Type>;

  private readonly publishedPromise: ManagedPromise<PointModelResolution>

  public get promise() {
    return this.publishedPromise.corePromise;
  }

  constructor(model: ContactModel, source: SimpleGestureSource<Type>) {
    /* c8 ignore next 3 */
    if(!model || !source) {
      throw new Error("A gesture-path source and contact-path model must be specified.");
    }

    this.model = model;
    this.publishedPromise = new ManagedPromise<PointModelResolution>();
    this.source = source;

    if(model.timer) {
      this.timerPromise = new TimeoutPromise(model.timer.duration);

      this.publishedPromise.then(() => {
        this.timerPromise.resolve(false);
        // but make sure that simultaneous path resolution continues even if the timer's is mismatched.
      });

      this.timerPromise.then((result) => {
        this.finalize(result == model.timer.expectedResult);
      });
    }
  }

  private finalize(result: boolean) {
    if(this.publishedPromise.isFulfilled) {
      return;
    }

    const model = this.model;
    if(result) {
      if(typeof model.pathResolutionAction == 'string') {
        this.publishedPromise.resolve({type: model.pathResolutionAction});
      } else {
        this.publishedPromise.resolve(model.pathResolutionAction);
      }
    } else {
      this.publishedPromise.resolve({type: 'reject'});
    }
  }

  update() {
    const model = this.model;
    const source = this.source;

    if(source.path.wasCancelled) {
      this.finalize(false);
      return 'reject';
    }

    if(model.itemChangeAction && source.currentHoveredItem != source.initialHoveredItem) {
      const result = model.itemChangeAction == 'resolve';

      this.finalize(result);
      return result;
    } else {
      // Note:  is current path, not 'full path'.
      const result = model.pathModel.evaluate(source.path) || 'continue';

      if(result != 'continue') {
        this.finalize(result == 'resolve');
      } else if(source.path.isComplete) {
        // If the PathModel said to 'continue' but the path is done, we default
        // to rejecting the model; there will be no more changes, after all.
        return 'reject';
      }

      return result;
    }
  }
}