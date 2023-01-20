import VirtualizedWorker from "./virtualizedWorker.js";
import unwrap from '../unwrap.js';

import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/build/lib/worker-main.wrapped-for-bundle.js";

export default class DefaultWorker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);
    if(true) { // If this is definitively set to either true or false, tree-shaking can take effect.
      scriptStr += '\n' + LMLayerWorkerSourcemapComment;
    }
    let worker = new VirtualizedWorker(scriptStr);

    return worker as any as Worker;
  }
}