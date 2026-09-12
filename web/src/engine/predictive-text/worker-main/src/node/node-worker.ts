/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { MappedWorker } from "./mappedWorker.js";

// TODO: eliminate MappedWorker as its own thing
export class NodeWorker {
  static constructInstance(): Worker {
    return new MappedWorker();
  }
}