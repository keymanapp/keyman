export { LMLayer } from '../lmlayer.js';
export { WebWorker } from './web-worker.js';

export interface WorkerFactory {
  constructInstance(): Worker
}
