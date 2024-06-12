import { timedPromise } from "@keymanapp/web-utils";

const MIN_OUTLIER = 1; // 1ms.
const MAX_CANDIDATES = 5;

export class ExecutionBucket {
  // Could make these readonly outside via getter...
  // but the class isn't exposed outside of the timer.
  // No need to worry.
  timeSpent: number = 0;
  eventCount: number = 0;

  private timeSquared: number = 0;

  /**
   * Used to track intervals in which potential context swaps by the OS may
   * have occurred.  Context switches generally seem to pause threads for
   * at least 16 ms, while we expect each loop iteration to complete
   * within just 1 ms.  So, any possible context switch should have the
   * longest observed change in system time.
   *
   * They are sorted in descending order so that the smallest potential outlier
   * is always accessible.
   */
  private nearOutliers: number[] = [];

  private outliers: number[] = [];

  private preventOutliers: boolean = false;

  constructor(preventOutliers?: boolean) {
    this.preventOutliers = !!preventOutliers;
  }

  add(time: number) {
    if(time < 0) {
      throw new Error("time may not be negative");
    }

    this.eventCount++;
    this.timeSpent += time;
    this.timeSquared += time * time;

    // As a safety, to prevent the outlier detection from getting too
    // aggressive, we set a flat minimum time threshold for something to be
    // considered an outlier.
    if(time >= MIN_OUTLIER && (this.nearOutliers.length < MAX_CANDIDATES || (this.nearOutliers[MAX_CANDIDATES-1] < time))) {
      this.nearOutliers.push(time);
      // sort in descending order
      this.nearOutliers.sort((a, b) => b-a);
    }

    this.checkForOutlier();
  }

  /**
   * Performs outlier detection based upon the Student's t-test distribution.
   * Only one candidate will be evaluated per call.
   * @returns
   */
  private checkForOutlier() {
    // We won't allow cases that result in less than 3 observations left after excluding
    // outliers; the stats-requirement for outlier detection at that point is too extreme.
    // We can always check candidate observations again when we have enough other samples.
    if(this.preventOutliers || this.eventCount < 4 || this.nearOutliers.length == 0) {
      return;
    }

    // For consideration:  the largest outlier candidate.
    // It wasn't ruled out in any previous pass, so neither were any smaller ones.
    //
    // Checking only one candidate per time observation helps keep this simpler
    // than it'd otherwise be.
    const possOutlier = this.nearOutliers[0];

    // For outlier comparison, temporarily remove them from the accumulated
    // stats.  They'd heavily skew the stats otherwise.
    this.timeSpent -= possOutlier;
    this.timeSquared -= possOutlier * possOutlier;
    this.eventCount--;

    // And now we do stats.  How far from the average IS the candidate,
    // relative to the variance without it present?
    const avg = this.average;
    const delta = possOutlier - avg;
    const variance = this.variance;

    // Calculated this way to avoid the expense of a Math.sqrt.
    const squaredDeviations = (delta * delta) / variance;

    // We could go more granular with this check, but that'd add complexity.
    // This should be "good enough".  Gives us 99% confidence in our decision.
    //
    // There is potential to accidentally exclude ~ 1 per 100 non-outliers;
    // that's the meaning of "99% confidence".  It's not ideal, but it's also a
    // comparatively small fraction of the total.  There's always the issue of
    // 'false positives' vs 'false negatives', and requiring more confidence
    // will increase 'false negatives'.  The MIN_OUTLIER threshold used in
    // `add()` aims to mitigate 'false positives' based on the tendency for
    // OS-triggered context switches to be on the order of milliseconds.
    //
    // Reference for values used: https://www.tdistributiontable.com/ (Or almost
    // any textbook for statistics majors/minors.)
    //
    // See the "t .99" column.  "df" = 1 less than non-outlier count.
          /* precise: 6.965 */                 /* precise: 2.998 */
    /* 3-7: > 7 times std dev */     /* 8+ non-outliers: > 3 times std dev */
    if(squaredDeviations >= 49 || (this.eventCount >= 8 && squaredDeviations >= 9)) {
      // we now consider the largest 'potential outlier' an actual outlier.
      //
      // At 7 "degrees of freedom" (8 non-outlier observations) a sample has
      // only a 0.5% chance of lying at or past 3 * the standard deviation - or
      // 9 times the variance.  2.998 would be more precise ("one tail",
      // p-factor 0.01, df = 7), but 3's "close enough".
      //
      // With just 3 non-outlier observations, we need a factor of 7 instead of
      // 3.  We'll allow it because actual 'predicting' should have a low total
      // count; 'correcting' will have significantly more observations.  6.965
      // would be more precise ("one tail", p-factor 0.01, df = 2), but 3's
      // "close enough".
      this.nearOutliers.shift();
      this.outliers.push(possOutlier);
      // // Useful for seeing how the settings look with real timings when running full
      // // unit test suite.
      // console.log(`detected outlier: ${possOutlier}`);
      // console.log(`avg: ${avg}, variance: ${variance}, eventCount: ${this.eventCount}`);
    } else {
      // Restore it; we decided it's not an outlier.
      //
      // We might lose least-significant-digit numerical precision due to manipulating
      // these values in this manner, but we don't need perfection here.
      this.timeSpent   += possOutlier;
      this.timeSquared += possOutlier * possOutlier;
      this.eventCount++;
    }
  }

  get average(): number {
    return this.timeSpent / this.eventCount;
  }

  get variance(): number {
    const N = this.eventCount;
    if(N <= 1) {
      return NaN;
    }

    // easy, efficient variance computation.
    return this.timeSquared / N - (this.timeSpent * this.timeSpent) / (N*N);
  }

  get outlierTime(): number {
    let sum = 0;
    for(let i=0; i < this.outliers.length; i++) {
      sum += this.outliers[i];
    }
    return sum;
  }
}

export class ExecutionSpan {
  private start: number;
  private finish?: number;
  private bucket: ExecutionBucket;
  private finalizer: () => void;

  constructor(bucket: ExecutionBucket, finalizer: () => void) {
    this.bucket = bucket;
    this.finalizer = finalizer;
    this.start = performance.now();
  }

  end() {
    this.finish = performance.now();
    this.bucket.add(this.duration);
    this.finalizer();
  }

  // Useful for tracking 'time since yield', etc.
  get duration() {
    return (this.finish ?? performance.now()) - this.start;
  }
}

/**
 * This is designed to help the correction-search algorithm detect its active
 * execution time.  While there's no official JS way to do this, we can
 * approximate it by polling the current system time (in ms) after each
 * iteration of a short-duration loop.  Unusual spikes in system time for a
 * single iteration is likely to indicate that an OS context switch occurred at
 * some point during the iteration's execution.
 *
 * Note: `.elapsedTime` + `.deferTime` may not sum up to the true total time spent;
 * the time spent between `.time()`, `.defer()`, and `start`-`end` timings is not
 * itself tracked and included, though that time should generally be minimal.
 */
export class ExecutionTimer {
  /**
   * The system time when this instance was created.
   */
  private trueStart: number;

  private buckets: Record<number, ExecutionBucket> = {};
  private deferBucket: ExecutionBucket = new ExecutionBucket(true /* prevent outliers */);

  private activeSpan: ExecutionSpan = null;

  private maxExecutionTime: number;
  private maxTrueTime: number;

  // TODO:  (next PR) track "time since last yield"?
  // That'd make a decent condition for yielding control briefly to the message-loop.

  constructor(maxExecutionTime: number, maxTrueTime: number) {
    this.trueStart = performance.now(); // is in ms.
    this.maxExecutionTime = maxExecutionTime;
    this.maxTrueTime = maxTrueTime;
  }

  /**
   * Used to enforce the specification set by `start()` - if a
   * previously-`start`ed span is not completed, it will throw an error.
   */
  private validateStart() {
    if(this.activeSpan) {
      throw new Error("illegal state - span-based timer still pending");
    }
  }

  /**
   * Gets the 'timing bucket' requested by the "timing set ID", creating it if
   * necessary.
   * @param timingSetId
   * @returns
   */
  private getBucket(timingSetId?: number): ExecutionBucket {
    timingSetId ??= -1;

    let bucket = this.buckets[timingSetId];
    if(!bucket) {
      bucket = this.buckets[timingSetId] = new ExecutionBucket(/* allow outliers */);
    }

    return bucket;
  }

  /**
   * The total amount of time spent executing.  Cases where extraordinarily
   * high amounts of time were spent are excluded as outliers.
   *
   * Does not include time spent since the last `.start()` call if the
   * corresponding `.end()` call has not yet occurred.
   */
  get executionTime(): number {
    const buckets = Object.values(this.buckets);
    let total = 0;
    for(let bucket of buckets) {
      total += bucket.timeSpent;
    }

    return total;
  }

  /**
   * The total amount of time waited during `defer`.  Cases where extraordinarily
   * high amounts of time were spent during execution are included here, as
   * outliers are considered to have been the result of context-switching
   * that would background their corresponding task.
   */
  get deferredTime(): number {
    const buckets = Object.values(this.buckets);
    let total = 0;

    for(let bucket of buckets) {
      total += bucket.outlierTime;
    }

    total += this.deferBucket.timeSpent;

    return total;
  }

  /**
   * This may be used to time a method's execution.  The original return value
   * will be preserved and passed through.
   *
   * Use set identifiers to ensure that outlier logic only applies among
   * observations of the same task type, as different tasks naturally take
   * different amounts of time.
   * @param closure  The method to time
   * @param timingSetId   A numerical identifier for the 'class' of things being
   *                      timed.  If not set, defaults to -1.
   * @returns
   */
  time<Type>(closure: () => Type, timingSetId?: number): Type {
    this.validateStart();

    const start = performance.now();
    const result = closure();
    const time = performance.now() - start;

    const bucket = this.getBucket(timingSetId);
    bucket.add(time);

    return result;
  }

  /**
   * This may be called to defer control to the base JS message loop /
   * task queue, resuming after all current entries are processed.
   *
   * The call will track the amount of time spent 'paused' due to this
   * deferment and will not count it against 'elapsed' time unless in
   * overly-high quantities.
   *
   * @param minWait Minimum time to wait before resuming.
   */
  async defer(minWait?: number) {
    this.validateStart();
    minWait ??= 0;

    const start = performance.now();
    // WebWorker messages appear to come in via the macrotask queue.
    await timedPromise(minWait);
    const time = performance.now() - start;

    this.deferBucket.add(time);
  }

  /**
   * Creates a split 'span' timer for cases where a closure is not viable.
   * Call the returned object's `end` method to finalize the timing span.
   *
   * All timing methods will throw errors when called if a 'span' from this
   * method is left unfinalized.
   *
   * Use set identifiers to ensure that outlier logic only applies among
   * observations of the same task type, as different tasks naturally take
   * different amounts of time.
   * @param timingSetId   A numerical identifier for the 'class' of things being
   *                      timed.  If not set, defaults to -1.
   * @returns  An object used to complete the "timing span" started by this function call.
   */
  start(timingSetId?: number): ExecutionSpan {
    this.validateStart();

    const bucket = this.getBucket(timingSetId);

    this.activeSpan = new ExecutionSpan(bucket, () => {
      this.activeSpan = null;
    });

    return this.activeSpan;
  }

  // TODO:  In follow-up PR:  add `terminate()` to force early termination
  // Also, rework correction-search to take an ExecutionTimer, not just the raw max length.
  // From there, can have new predict calls call `.terminate()` on the prior call's timer.

  /**
   * Returns `true` if the time interval represented by this timer should be considered
   * as fully elapsed.
   * @returns
   */
  get elapsed(): boolean {
    const now = performance.now();
    if(now - this.trueStart >= this.maxTrueTime) {
      return true;
    }

    return this.executionTime >= this.maxExecutionTime;
  }
}