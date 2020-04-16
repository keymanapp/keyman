/// <reference path="lmlayer-interface.ts" />
/// <reference path="embedded_worker.d.ts" />
/// <reference path="worker-interface.d.ts" />

namespace com.keyman.text.prediction {
  // Unfortunately, importing like this loses all type information for the modules.
  // It's that, or massively overhaul lm-layer to be module-based instead of
  // namespace-based.
  var fs = require("fs");
  var vm = require("vm");

  class VirtualizedWorkerContext {
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

  /**
   * Note:  this does not create an actual Worker, separate process, or thread.  Everything will
   *        be executed in-line on a virtualized context.
   * 
   *        In the future, it might be nice to use Node's Worker Threads implementation.
   */
  export class VirtualizedWorker implements Worker {
    private _worker: any;  // Really, the LMLayerWorker... but some of its types are problematic for headless.
    private _workerContext: VirtualizedWorkerContext;

    constructor() {
      this._workerContext = new VirtualizedWorkerContext();
      // Needs to exist before setting up the worker; must exist by `.install()`.
      this._workerContext.postMessage = this.workerPostMessage.bind(this);

      // Initialize the "worker".
      let script = LMLayerBase.unwrap(LMLayerWorkerCode);
      this._workerContext.__importScriptString(script);
      this._workerContext['LMLayerWorker'].install(this._workerContext);

      // Establish the bridge between worker and outer layer.
      this._worker = this._workerContext['LMLayerWorker'];
    }

    // Sends the worker's postMessage messages to the appropriate `onmessage` handler.
    private workerPostMessage(message: any, options: any) {
      if(this.onmessage) {
        this.onmessage({data: message} as any as MessageEvent);
      }
    }

    onmessage: (this: Worker, ev: MessageEvent) => any;

    postMessage(message: any, transfer: any[]): void;
    postMessage(message: any, options?: PostMessageOptions): void;
    postMessage(message: any, options?: any) {
      let msgObj = {data: message};
      let msgJSON = JSON.stringify(msgObj);

      /* 
       * Execute the command within the virtualized worker's scope.  The worker's returned 
       * `postMessage` calls will still reach outside, as they have a reference to `this` via
       * `postMessage` (which we've set to a bound `this.workerPostMessage`).
       * 
       * Among other things, this will allow the worker to use its internal namespaces without issue.
       */
      let msgCommand = "onmessage(" + msgJSON + ")";
      this._workerContext.__importScriptString(msgCommand);
    }

    terminate(): void {
      this._workerContext = null;
    }
  }

  export class VirtualizedWorkerFactory implements com.keyman.text.prediction.WorkerFactory {
    constructInstance(): Worker {
      return new VirtualizedWorker();
    }
  }
}