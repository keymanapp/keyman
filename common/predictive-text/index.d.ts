/**
 * Defines TS method typing for the LMLayer's public API.
 *
 * Unfortunately, it is not possible to do this programmatically at this time, as TS won't output declarations
 * if any JS files are allowed in compilation.  We can't convert the embedded worker's intermediate file to TS
 * safely due to DedicatedWorkerGlobalScope and a few other typing issues.
 */

/// <reference types="@keymanapp/lm-message-types" />
/// <reference types="@keymanapp/models-types" />

declare namespace com.keyman.text.prediction {
  class LMLayer {

    /**
     * Construct the top-level LMLayer interface. This also starts the underlying Worker.
     * Make sure to call .load() when using the default Worker.
     *
     * @param uri URI of the underlying LMLayer worker code. This will usually be a blob:
     *            or file: URI. If uri is not provided, this will start the default Worker.
     */
    constructor(capabilities: Capabilities, worker?: Worker);

    /**
     * Initializes the LMLayer worker with the keyboard/platform's capabilities,
     * as well as a description of the model required.
     */
    loadModel(model: string): Promise<Configuration>;

    /**
     * Prepares the LMLayer for reinitialization with a different model/capability set.
     */
    unloadModel();

    predict(transform: Transform, context: Context): Promise<Suggestion[]>;

    // TODO: asynchronous close() method.
    //       Worker code must recognize message and call self.close().

    private onMessage(event: MessageEvent): void;

    /**
     * Given a function, this utility returns the source code within it, as a string.
     * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
     * build process.
     *
     * @param fn The function whose body will be returned.
     */
    static unwrap(fn: Function): string;

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
    static asBlobURI(fn: Function): string;

    /**
     * Clears out any computational resources in use by the LMLayer, including shutting
     * down any internal WebWorkers.
     */
    public shutdown(): void;
  }
}