/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export class WebWorker {
  static constructInstance(): Worker {
    return new Worker(this.workerURI());
  }

  static workerURI(): string {
    // TODO: worker-thread generates worker-main.js,... whaaa
    // TODO: worker-main.min.js?
    // TODO: paths
    return './worker-main.js';
  }
}