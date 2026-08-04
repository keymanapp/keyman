/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { MappedWorker } from "./mappedWorker.js";
import { unwrap } from '../unwrap.js';

import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/worker-main.wrapped.min.js";

export class NodeWorker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);
    const srcMapUrlIndex = scriptStr.indexOf("//# sourceMappingURL")
    if(srcMapUrlIndex > 0) {
      scriptStr = scriptStr.substring(0, srcMapUrlIndex);
    }

    scriptStr += '\n' + LMLayerWorkerSourcemapComment;
    return new MappedWorker(scriptStr);
  }
}