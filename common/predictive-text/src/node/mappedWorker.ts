// We use a subset of the Worker interface here; compiling directly against the true
// WebWorker type definitions would require us to implement more methods than we do.
/// <reference path="../worker-interface.d.ts" />

// Defines types related to Node workers.
import * as worker from 'worker_threads';
import { Buffer } from 'buffer';
import { URL } from 'url';

/**
 * Defines mappings from Node Worker signatures to WebWorker signatures
 */
const nodeWorkerToWebWorkerMappingSource = `
import { parentPort } from 'worker_threads';
import fs from 'fs';
import vm from 'vm';

function postMessage(...args) {
  parentPort.postMessage.call(parentPort, args);
}

parentPort.on('message', (ev) => {
  onmessage({data: ev});
});

function importScripts(...args) {
  function loadScriptInContext(scriptPath) {
    let scriptStr = fs.readFileSync(scriptPath);
    var script = new vm.Script(scriptStr, { filename: scriptPath });
    script.runInThisContext();
  }

  for(let arg of args) {
    loadScriptInContext(arg);
  }
}

/*
 * You'd think the method signature mapping would be implied from the first line,
 * but all three lines must be explicitly specified or the emulation will fail.
 */
const self = globalThis;
self.postMessage = postMessage;
self.importScripts = importScripts;
`;

/**
 * Uses the Node version of Workers to provide proper, authentic separate-thread
 * 'sandboxing'.  Also intercepts and interprets certain WebWorker method signatures
 * necessary to run the WebWorker-oriented worker code.
 */
export default class MappedWorker extends worker.Worker implements Worker {
  constructor(scriptStr: string) {
    const concatenatedScript = `
    ${nodeWorkerToWebWorkerMappingSource}

    ${scriptStr}
    `;
    const buffer = Buffer.from(concatenatedScript);
    const dataSrc = "data:text/javascript;base64," + buffer.toString('base64');
    //@ts-ignore
    super(new URL(dataSrc));

    // WebWorkers have a defined `onmessage` function, rather than this.on('message', ...)
    this.on('message', (ev) => {
      if(this.onmessage) {
        this.onmessage({data: ev[0]});
      }
    });
  }

  /**
   * Accepts a callback function that will receive messages sent from the `VirtualizedWorker`'s `postMessage` function,
   * much like the standard `Worker.onmessage`.
   */
  onmessage: (this: Worker, ev: MessageEvent) => any;
}