namespace com.keyman.text.prediction {
  export class WebWorkerFactory implements WorkerFactory {
    constructInstance(): Worker {
      return new Worker(WebWorkerFactory.asBlobURI(LMLayerWorkerCode));
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
    static asBlobURI(fn: Function): string {
      let code = LMLayerBase.unwrap(fn);
      let blob = new Blob([code], { type: 'text/javascript' });
      return URL.createObjectURL(blob);
    }
  }
}