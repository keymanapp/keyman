/// <reference path="lmlayer-interface.ts" />
/// <reference path="embedded_worker.d.ts" />
/// <reference path="worker-interface.d.ts" />

import fs = require("fs");
import vm = require("vm");

class HeadlessWorkerContext {
  postMessage: (message: any, options: PostMessageOptions) => void;

  importScripts() {
    /* Use of vm.createContext and script.runInContext allow us to avoid
      * polluting the global scope with imports.  When we throw away the
      * context object, imported scripts will be automatically GC'd.
      */
    for(var i=0; i < arguments.length; i++) {
      let context = vm.createContext(this);
      var script = new vm.Script(fs.readFileSync(arguments[i]) as any as string);
      script.runInContext(context);
    }
  }
}

export class HeadlessWorker implements Worker {
  private _worker: any;  // Really, the LMLayerWorker... but some of its types are problematic for headless.
  private _workerContext: HeadlessWorkerContext;

  constructor() {
    this._workerContext = new HeadlessWorkerContext();
    (function(){
      LMLayerWorkerCode();
      //@ts-ignore
      LMLayerWorker.install(this._workerContext);
    }());

    this._worker = this._workerContext['LMLayerWorker'];
    this._workerContext.postMessage = this.transferMessage.bind(this);
  }

  private transferMessage(message: any, options: any) {
    let messageEvent = {
      data: {
        message: message
      }
    }

    for(let key in options) {
      messageEvent[key] = options[key];
    }

    if(this.onmessage) {
      this.onmessage(message);
    }
  }

  onmessage: (this: Worker, ev: MessageEvent) => any;

  postMessage(message: any, transfer: any[]): void;
  postMessage(message: any, options?: PostMessageOptions): void;
  postMessage(message: any, options?: any) {
    let messageEvent = {
      data: {
        message: message
      }
    }

    for(let key in options) {
      messageEvent[key] = options[key];
    }

    this._worker.onMessage(messageEvent as MessageEvent);
  }

  terminate(): void {
    this._workerContext = null;
  }
}

export class HeadlessWorkerFactory implements com.keyman.text.prediction.WorkerFactory {
  constructInstance(): Worker {
    return new HeadlessWorker();
  }
}