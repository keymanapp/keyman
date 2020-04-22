///<reference path="virtualizedWorker.ts" />

namespace com.keyman.text.prediction {
  export class DefaultWorker {
    static constructInstance(): Worker {
      let scriptStr = LMLayer.unwrap(LMLayerWorkerCode);
      let worker = new VirtualizedWorker(scriptStr);

      return worker;
    }
  }
}