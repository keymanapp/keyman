import MappedWorker from "./mappedWorker.js";
import unwrap from '../unwrap.js';

import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/worker-main.wrapped.min.js";

export default class Worker {
  static constructInstance(): Worker {
    let scriptStr = unwrap(LMLayerWorkerCode);
    const srcMapUrlIndex = scriptStr.indexOf("//# sourceMappingURL")
    if(srcMapUrlIndex > 0) {
      scriptStr = scriptStr.substring(0, srcMapUrlIndex);
    }

    scriptStr += '\n' + LMLayerWorkerSourcemapComment;
    let worker = new MappedWorker(scriptStr);

    return worker as any as Worker;
  }
}