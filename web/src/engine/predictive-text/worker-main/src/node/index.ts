export { LMLayer } from '../lmlayer.js';
export { NodeWorkerFactory } from './node-worker.js';

export interface WorkerFactory {
    constructInstance(workerSource: string): Worker
  }