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
      console.log("Executing: " + functor.toString());
      //console.log("Queue: " + JSON.stringify(this.queue.map((functor) => functor.toString()), null, 2));
      console.log("Queue length: " + this.queue.length);
      // Is either undefined or is a Promise.
      try {
        const result = functor();
        if(result) {
          const withMsg = result.then(() => {
            console.log("Specialized deferment complete: " + functor.toString())
            // return timedPromise(0);
          });
          this.setDeferment(withMsg);
          return;
        } else {
          const defaultDelay = timedPromise(0);
          const withMsg = defaultDelay.then(() => console.debug("triggerEvent - microtask queue clear"));
          this.setDeferment(withMsg);
        }
      } catch (err) {
        console.error("Error sequentializing received inputs");
        console.error(err);
      }

      // // Allow the microtask queue to clear out before proceeding.
      // //
      // // With the exception of 'inputstart' events, all related gesture processing is dependent
      // // on the microtask queue.  As for events that should follow an 'inputstart', they're
      // // guarded via ManagedPromise-based locks established within InputEventEngine and
      // // unlocked only after the 'inputstart' event itself signals.

      // const stdDeferment = timedPromise(0).then(() => timedPromise(0));
      // stdDeferment.then(() => console.log("Std deferment complete: " + functor.toString()));
      // this.setDeferment(stdDeferment);

      // await timedPromise(0);
      // console.log("timedPromise 1: " + functor.toString());
      // await timedPromise(0);
      // console.log("timedPromise 2: " + functor.toString());
    }
  }

  queueEventFunctor(functor: () => Promise<void> | void) {
    console.log("Queuing: " + functor.toString());
    this.queue.push(functor);
    // We only need to trigger events if the queue has no prior entries and there isn't an
    // active deferment that will auto-trigger the event at the appropriate time.
    if(this.queue.length == 1 && !this.defermentPromise) {
      this.triggerEvent();
    }
  }
}