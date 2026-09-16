export { LMLayer } from '../lmlayer.js';
export { WebWorkerFactory } from './web-worker.js';

export interface WorkerFactory {
  constructInstance(workerSource: string): Worker
}
