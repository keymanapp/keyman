import { timedPromise } from "@keymanapp/web-utils";

export class EventSequentializationQueue {
  private queue: (() => Promise<void> | void)[];
  private defermentPromise: Promise<void>;

  constructor() {
    this.queue = [];
  }

  private setDeferment(promise: Promise<any>) {
    this.defermentPromise = promise;
    promise.then(() => {
      this.defermentPromise = null;
      this.triggerEvent();
    });
  }

  private async triggerEvent() {
    while(this.queue.length > 0) {
      const functor = this.queue.shift();

      // Things break _badly_ if we don't keep the queue running if errors are triggered by the functor.
      // It's best to ignore the error and let things play out.
      try {
        // Is either undefined or is a Promise.
        const result = functor();
        // We either wait on a manual lock (from within an InputEventEngine) or a macrotask queue wait,
        // allowing gesture-matching microtask queue Promises to complete before proceeding.
        this.setDeferment(result ? result : timedPromise(0));
      } catch (err) {
        const baseMsg = 'Error sequentializing received inputs';
        if(err instanceof Error) {
          console.error(`${baseMsg}: ${err.message}\n\n${err.stack}`);
        } else {
          console.error(baseMsg);
          console.error(err);
        }
      }
    }
  }

  queueEventFunctor(functor: () => Promise<void> | void) {
    this.queue.push(functor);
    // We only need to trigger events if the queue has no prior entries and there isn't an
    // active deferment that will auto-trigger the event at the appropriate time.
    if(this.queue.length == 1 && !this.defermentPromise) {
      this.triggerEvent();
    }
  }
}