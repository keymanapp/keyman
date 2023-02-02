// We use a subset of the Worker interface here; compiling directly against the true
// WebWorker type definitions would require us to implement more methods than we do.
/// <reference path="../worker-interface.d.ts" />

import * as fs from 'fs';
import * as vm from 'vm';

class VirtualizedWorkerContext {
  // The LMLayerWorker installs itself to 'self', the expected Worker global, so we provide an alias.
  self: VirtualizedWorkerContext;

  constructor() {
    this.self = this;
  }

  postMessage: (message: any) => void;

  importScripts(...scriptNames: string[]) {
    /* Use of vm.createContext and script.runInContext allow us to avoid
      * polluting the global scope with imports.  When we throw away the
      * context object, imported scripts will be automatically GC'd.
      */
    for(let script of scriptNames) {
      this.__importScriptString(fs.readFileSync(script, "UTF-8"));
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
export default class VirtualizedWorker implements Worker {
  private _workerContext: VirtualizedWorkerContext;

  constructor(scriptStr: string) {
    this._workerContext = new VirtualizedWorkerContext();
    // Needs to exist before setting up the worker; must exist by `.install()`.
    this._workerContext.postMessage = this.workerPostMessage.bind(this);

    // Initialize the "worker".
    this._workerContext.__importScriptString(scriptStr);
  }

  // Sends the worker's postMessage messages to the appropriate `onmessage` handler.
  private workerPostMessage(message: unknown) {
    if(this.onmessage) {
      this.onmessage({data: message} as any as MessageEvent);
    }
  }

  /**
   * Accepts a callback function that will receive messages sent from the `VirtualizedWorker`'s `postMessage` function,
   * much like the standard `Worker.onmessage`.
   */
  onmessage: (this: Worker, ev: MessageEvent) => any;

  postMessage(message: any) {
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