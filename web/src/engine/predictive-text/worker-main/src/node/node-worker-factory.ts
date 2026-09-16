/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { WorkerFactory } from "../worker-factory.js";
import { NodePredictiveTextWorker } from "./node-predictive-text-worker.js";

export class NodePredictiveTextWorkerFactory implements WorkerFactory {
  constructInstance(_rootPath?: string): Worker {
    // note: rootPath is never used for Node worker, as it is only instanatiated
    // in unit tests, where paths are known
    return new NodePredictiveTextWorker();
  }
}

export const nodePredictiveTextWorkerFactory = new NodePredictiveTextWorkerFactory();
