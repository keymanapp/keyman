/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { WorkerFactory } from "../worker-factory.js";

export class WebPredictiveTextWorkerFactory implements WorkerFactory {
  constructor(private workerFilename: string) {}
  constructInstance(): Worker {
    return new Worker(this.workerFilename);
  }
}
