export { LMLayer } from './lmlayer.js';
export { NodeWorker } from './node/node-worker.js';
export { WebWorker } from './web/web-worker.js';

export interface WorkerFactory {
  constructInstance(): Worker
}