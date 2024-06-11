/**
 * This inner class is designed to help the algorithm detect its active execution time.
 * While there's no official JS way to do this, we can approximate it by polling the
 * current system time (in ms) after each iteration of a short-duration loop.  Unusual
 * spikes in system time for a single iteration is likely to indicate that an OS
 * context switch occurred at some point during the iteration's execution.
 */
export class ExecutionTimer {
  /**
   * The system time when this instance was created.
   */
  private start: number;

  /**
   * Marks the system time at the start of the currently-running loop, as noted
   * by a call to the `startLoop` function.
   */
  private loopStart: number;

  private maxExecutionTime: number;
  private maxTrueTime: number;

  private executionTime: number;

  /**
   * Used to track intervals in which potential context swaps by the OS may
   * have occurred.  Context switches generally seem to pause threads for
   * at least 16 ms, while we expect each loop iteration to complete
   * within just 1 ms.  So, any possible context switch should have the
   * longest observed change in system time.
   *
   * See `updateOutliers` for more details.
   */
  private largestIntervals: number[] = [0];

  constructor(maxExecutionTime: number, maxTrueTime: number) {
    // JS measures time by the number of milliseconds since Jan 1, 1970.
    this.loopStart = this.start = Date.now();
    this.maxExecutionTime = maxExecutionTime;
    this.maxTrueTime = maxTrueTime;
  }

  startLoop() {
    this.loopStart = Date.now();
  }

  markIteration() {
    const now = Date.now();
    const delta = now - this.loopStart;
    this.executionTime += delta;

    /**
     * Update the list of the three longest system-time intervals observed
     * for execution of a single loop iteration.
     *
     * Ignore any zero-ms length intervals; they'd make the logic much
     * messier than necessary otherwise.
     */
    if(delta && delta > this.largestIntervals[0]) {
      // If the currently-observed interval is longer than the shortest of the 3
      // previously-observed longest intervals, replace it.
      if(this.largestIntervals.length > 2) {
        this.largestIntervals[0] = delta;
      } else {
        this.largestIntervals.push(delta);
      }

      // Puts the list in ascending order.  Shortest of the list becomes the head,
      // longest one the tail.
      this.largestIntervals.sort();

      // Then, determine if we need to update our outlier-based tweaks.
      this.updateOutliers();
    }
  }

  updateOutliers() {
    /* Base assumption:  since each loop of the search should evaluate within ~1ms,
     *                   notably longer execution times are probably context switches.
     *
     * Base assumption:  OS context switches generally last at least 16ms.  (Based on
     *                   a window.setTimeout() usually not evaluating for at least
     *                   that long, even if set to 1ms.)
     *
     * To mitigate these assumptions:  we'll track the execution time of every loop
     * iteration.  If the longest observation somehow matches or exceeds the length of
     * the next two almost-longest observations twice over... we have a very strong
     * 'context switch' candidate.
     *
     * Or, in near-formal math/stats:  we expect a very low variance in execution
     * time among the iterations of the search's loops.  With a very low variance,
     * ANY significant proportional spikes in execution time are outliers - outliers
     * likely caused by an OS context switch.
     *
     * Rather than do intensive math, we use a somewhat lazy approach below that
     * achieves the same net results given our assumptions, even when relaxed somewhat.
     *
     * The logic below relaxes the base assumptions a bit to be safe:
     * - [2ms, 2ms, 8ms]  will cause 8ms to be seen as an outlier.
     * - [2ms, 3ms, 10ms] will cause 10ms to be seen as an outlier.
     *
     * Ideally:
     * - [1ms, 1ms, 4ms] will view 4ms as an outlier.
     *
     * So we can safely handle slightly longer average intervals and slightly shorter
     * OS context-switch time intervals.
     */
    if(this.largestIntervals.length > 2) {
      // Precondition:  the `largestIntervals` array is sorted in ascending order.
      // Shortest entry is at the head, longest at the tail.
      if(this.largestIntervals[2] >= 2 * (this.largestIntervals[0] + this.largestIntervals[1])) {
        this.executionTime -= this.largestIntervals[2];
        this.largestIntervals.pop();
      }
    }
  }

  shouldTimeout(): boolean {
    const now = Date.now();
    if(now - this.start > this.maxTrueTime) {
      return true;
    }

    return this.executionTime > this.maxExecutionTime;
  }

  resetOutlierCheck() {
    this.largestIntervals = [];
  }
}