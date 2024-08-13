import MappedWorker from "./mappedWorker.js";
import unwrap from '../unwrap.js';

import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/worker-main.wrapped.js";

export default class SourcemappedWorker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);

    // If this is definitively set to either true or false, tree-shaking can take effect.
    // An imported const variable doesn't seem to do it, though.
    // if(false) {
      scriptStr += '\n' + LMLayerWorkerSourcemapComment;
    // }
    let worker = new MappedWorker(scriptStr);

    return worker as any as Worker;
  }
}