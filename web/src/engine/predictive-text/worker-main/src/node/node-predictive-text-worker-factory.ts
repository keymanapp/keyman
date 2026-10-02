/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { WorkerFactory } from "../worker-factory.js";
import { NodePredictiveTextWorker } from "./node-predictive-text-worker.js";

import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename__ = fileURLToPath(import.meta.url);
const __dirname__ = path.dirname(__filename__);
export const nodeLexicalModelWorkerPath = __dirname__ + "/../../../../worker-thread/build/lib/worker-thread.js";

export class NodePredictiveTextWorkerFactory implements WorkerFactory {
  private workerFilename: string;
  constructor(workerFilename?: string) {
    this.workerFilename = workerFilename ?? nodeLexicalModelWorkerPath;
  }
  constructInstance(): Worker {
    return new NodePredictiveTextWorker(this.workerFilename);
  }
}
