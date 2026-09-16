/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { MappedWorker } from "./mappedWorker.js";

// TODO: eliminate MappedWorker as its own thing
export class NodeWorkerFactory {
  constructInstance(workerSource: string): Worker {
    return new MappedWorker(workerSource);
  }
}