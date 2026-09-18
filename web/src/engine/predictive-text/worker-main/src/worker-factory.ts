/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export interface WorkerFactory {
  constructInstance(rootPath?: string): Worker;
};