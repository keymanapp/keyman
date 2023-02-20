import unwrap from '../unwrap.js';
import { LMLayerWorkerCode, LMLayerWorkerSourcemapComment } from "@keymanapp/lm-worker/worker-main.wrapped.min.js";

export default class DefaultWorker {
  static constructInstance(): Worker {
    return new Worker(this.asBlobURI(LMLayerWorkerCode));
  }

  /**
   * Converts the INSIDE of a function into a blob URI that can
   * be passed as a valid URI for a Worker.
   * @param fn Function whose body will be referenced by a URI.
   *
   * This function makes the following possible:
   *
   *    let worker = new Worker(LMLayer.asBlobURI(function myWorkerCode () {
   *      postMessage('inside Web Worker')
   *      function onmessage(event) {
   *        // handle message inside Web Worker.
   *      }
   *    }));
   */
  static asBlobURI(encodedSrc: string): string {
    let code = unwrap(encodedSrc);

    // If this is definitively set to either true or false, tree-shaking can take effect.
    // An imported const variable doesn't seem to do it, though.
    // if(false) {
      code += '\n' + LMLayerWorkerSourcemapComment;
    // }
    let blob = new Blob([code], { type: 'text/javascript' });
    return URL.createObjectURL(blob);
  }
}