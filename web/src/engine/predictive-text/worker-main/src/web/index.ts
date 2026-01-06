export { LMLayer } from '../lmlayer.js';
export { DefaultWorker as Worker } from './worker.js';
export { SourcemappedWorker } from './sourcemappedWorker.js';

export interface WorkerFactory {
  constructInstance(): Worker
}
