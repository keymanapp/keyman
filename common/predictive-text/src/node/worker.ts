import VirtualizedWorker from "./virtualizedWorker.js";
import unwrap from '../unwrap.js';

import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/build/lib/worker-main.wrapped.min.js";

export default class Worker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);

    scriptStr += '\n' + LMLayerWorkerSourcemapComment;
    let worker = new VirtualizedWorker(scriptStr);

    return worker as any as Worker;
  }
}