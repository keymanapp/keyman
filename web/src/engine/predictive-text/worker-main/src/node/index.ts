export { LMLayer } from '../lmlayer.js';
export { NodeWorker } from './node-worker.js';

export interface WorkerFactory {
    constructInstance(): Worker
  }