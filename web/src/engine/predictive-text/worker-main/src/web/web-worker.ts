/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export class WebWorkerFactory {
  constructInstance(workerSource: string): Worker {
    return new Worker(workerSource);
  }
}