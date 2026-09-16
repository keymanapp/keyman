export { LMLayer } from './lmlayer.js';
export { NodeWorkerFactory } from './node/node-worker.js';
export { WebWorkerFactory } from './web/web-worker.js';

export interface WorkerFactory {
  constructInstance(workerSourcePath: string): Worker
}