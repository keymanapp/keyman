/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { WorkerFactory } from "../worker-factory.js";

export class WebPredictiveTextWorkerFactory implements WorkerFactory {
  constructInstance(rootPath?: string): Worker {
    if(!rootPath) {
      rootPath = './';
    }

    // #13862 - if we point to a combined worker+model filename then don't fixup
    // path. See KMManager.java for full details.
    //
    // TODO: this could be made unnecessary by tweaking the callers instead
    if(!rootPath.endsWith('.js')) {
      if(!rootPath.endsWith('/')) {
        rootPath = rootPath + '/';
      }
      rootPath = rootPath + 'worker-thread.js';
    }

    return new Worker(rootPath);
  }

  constructTestInstance(): Worker {
    return new Worker('/web/src/engine/predictive-text/worker-thread/build/lib/worker-thread.js');
  }
}

export const webPredictiveTextWorkerFactory = new WebPredictiveTextWorkerFactory();
