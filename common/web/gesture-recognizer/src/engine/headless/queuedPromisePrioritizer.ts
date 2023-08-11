interface Entry {
  priority: number,
  ordinal: number
}

interface SynchronizationSet {
  entries: Entry[],
  promise: Promise<void>
}

/**
 * The short version: this is a Promise "resynchronizer" that allows us to choose the order in which
 * "synchronous" promises resolve.
 *
 * To clarify: if one external function call triggers direct resolution of three separate Promises
 * that instantly queue against this class's `queueWithPriority` method, they are considered
 * "synchronous" - as they're all placed on the queue before the queue regains control and before
 * the first of the three Promises signals its resolution.
 *
 * If each of the three then awaits its turn via said queueing, they will each then resolve in order
 * of their specified priority.
 */
export class QueuedPromisePrioritizer {
  private currentSyncPromise: Promise<void>;
  private setQueue: SynchronizationSet[] = [];

  private ordinalSeed: number = 0;

  /**
   * This method generates a new Promise on the JS Promise queue that will resolve in custom order
   * relative to other calls against this method that occur as of a direct, synchronous result of
   * Promises already queued for fulfillment.
   * - If there are three such Promises queued that each call this method, the new Promise's
   *   "instant resolution" is queued (1) after the third call queues itself AND (2) after any
   *   of the others that queue with a higher priority level have received their turns.
   *
   * Any future calls caused by Promise fulfillment not yet queued up, as of the "first" (w.l.o.g)
   * call against this method, must await their turns until after the lowest priority call from
   * Promise fulfillments already queued has received its turn.
   * - To continue the same example, if a timeout of 1 ms is set after the third queuing call
   *   and only then a fourth call for promise-queuing occurs, the fourth will always resolve
   *   after all three prior calls resolve, no matter what priority value is supplied.
   *
   * @param priority
   */
  async queueWithPriority(priority: number) {
    // If we're not in the middle of recognizing a set of simultaneously-queued Promises,
    // time to enter that state.
    if(!this.currentSyncPromise) {
      const syncMetadata: SynchronizationSet = {
        entries: [],
        promise: this.currentSyncPromise = Promise.resolve()
      };

      /*
       * Once we've finished recording the set, we can sort the Promises by priority order - larger
       * values first.  No need to sort it every time, after all - no new entries will be allowed
       * at that point.
       */
      this.currentSyncPromise.then(() => {
        // Descending priority order for _our_ syncSet.  We should only need to do it once, and can do
        // it once collection is complete (see `await` below).
        syncMetadata.entries.sort((a, b) => {
          let primary = b.priority - a.priority;
          if(primary) {
            return primary;
          }

          // If equal priority, the one that registered first wins.  It's an easy enough guarantee
          // to make, so why not?
          return a.ordinal - b.ordinal;
        });
      });

      this.setQueue.push(syncMetadata);
    }

    // Capture the current sync-Promise
    const syncSet = this.setQueue[this.setQueue.length-1];

    // Register our tracking metadata for this queue request.
    const entry: Entry = {
      priority: priority,
      ordinal: this.ordinalSeed++
    }
    syncSet.entries.push(entry);

    // Now that we've recorded the request's metadata...
    await syncSet.promise; // which is Promise.resolve(), and thus already queued for fulfillment.

    /*
     * As the line above is the first `await` within this method, only now can other asynchronous
     * things have a chance to execute and/or add new Promises to the queue.  We've already registered
     * our synchronization Promise and reserved our slot in the Promise queue, allowing us to meet
     * our guarantees.
     *
     * As a result, registration is now over and done for Promises allowed to be treated as
     * synchronous.  Any new calls from new Promises - or even from resolution handlers for existing
     * ones - will always reach this point in this function later, effectively after our denoted
     * queue-split point, and definitely "asynchronously".
     *
     * By clearing the current syncSet marker, this function will know to start a new, separate `syncSet`.
     * (See:  this method's first code block / conditional on this field)
     */
    this.currentSyncPromise = null;

    // // #1 - make sure our sync set isn't deferred pending completion of a different syncSet.
      // #2 - if we're not the highest priority here... re-defer to let the higher-priority
      //      entries go first.
    while(this.setQueue[0].entries[0] != entry) {
      await syncSet.promise;
    }

    // It's our turn now!  But first, remove our entries in the queue to make sure the next guy
    // in the queue gets his turn / can exit the while-loop above.
    syncSet.entries.splice(0, 1);

    if(syncSet.entries.length == 0) {
      this.setQueue.splice(0, 1);
    }

    // And we're in the clear!
    return;
  }
}