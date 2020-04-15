/// <reference path="lmlayer-interface.ts" />
/// <reference path="embedded_worker.d.ts" />
/// <reference path="worker-interface.d.ts" />

namespace com.keyman.text.prediction {
  // Unfortunately, importing like this loses all type information for the modules.
  // It's that, or massively overhaul lm-layer to be module-based instead of
  // namespace-based.
  var fs = require("fs");
  var vm = require("vm");

  class HeadlessWorkerContext {
    // The LMLayerWorker assigns itself to 'window', so we provide an alias.
    window: any;

    constructor() {
      this.window = this;
    }

    postMessage: (message: any, options: PostMessageOptions) => void;

    importScripts() {
      /* Use of vm.createContext and script.runInContext allow us to avoid
        * polluting the global scope with imports.  When we throw away the
        * context object, imported scripts will be automatically GC'd.
        */
      for(var i=0; i < arguments.length; i++) {
        this.__importScriptString(fs.readFileSync(arguments[i]) as any as string);
      }
    }

    __importScriptString(scriptStr: string) {
      let context = vm.createContext(this);
      var script = new vm.Script(scriptStr);
      script.runInContext(context);
    }
  }

  export class HeadlessWorker implements Worker {
    private _worker: any;  // Really, the LMLayerWorker... but some of its types are problematic for headless.
    private _workerContext: HeadlessWorkerContext;

    constructor() {
      this._workerContext = new HeadlessWorkerContext();
      // Needs to exist before setting up the worker; must exist by `.install()`.
      this._workerContext.postMessage = this.transferMessage.bind(this);

      // Initialize the "worker".
      let script = LMLayerBase.unwrap(LMLayerWorkerCode);
      this._workerContext.__importScriptString(script);
      this._workerContext['LMLayerWorker'].install(this._workerContext);

      // Establish the bridge between worker and outer layer.
      this._worker = this._workerContext['LMLayerWorker'];
    }

    private transferMessage(message: any, options: any) {
      if(this.onmessage) {
        this.onmessage({data: message} as any as MessageEvent);
      }
    }

    onmessage: (this: Worker, ev: MessageEvent) => any;

    postMessage(message: any, transfer: any[]): void;
    postMessage(message: any, options?: PostMessageOptions): void;
    postMessage(message: any, options?: any) {
      this._worker.onMessage({data: message} as any as MessageEvent);
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
}