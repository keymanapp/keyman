import VirtualizedWorker from "./virtualizedWorker.js";
import unwrap from '../unwrap.js';

import LMLayerWorkerCode from "@keymanapp/lm-worker/build/lib/worker-main.wrapped-for-bundle.js";

export default class DefaultWorker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);
    let worker = new VirtualizedWorker(scriptStr);

    return worker as any as Worker;
  }
}