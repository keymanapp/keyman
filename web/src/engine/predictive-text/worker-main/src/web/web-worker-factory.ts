/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { WorkerFactory } from "../worker-factory.js";

export class WebPredictiveTextWorkerFactory implements WorkerFactory {
  constructInstance(rootPath?: string): Worker {
    if(!rootPath) {
      rootPath = './';
    }
    else if(!rootPath.endsWith('/')) {
      rootPath = rootPath + '/';
    }
    return new Worker(rootPath + 'worker-thread.js');
  }

  constructTestInstance(): Worker {
    return new Worker('/web/src/engine/predictive-text/worker-thread/build/lib/worker-thread.js');
  }
}

export const webPredictiveTextWorkerFactory = new WebPredictiveTextWorkerFactory();
