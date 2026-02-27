export { LMLayer } from '../lmlayer.js';
export { WebWorker } from './web-worker.js';
export { SourcemappedWorker } from './sourcemappedWorker.js';

export interface WorkerFactory {
  constructInstance(): Worker
}
