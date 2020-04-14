namespace com.keyman.text.prediction {
  export interface WorkerFactory {
    constructInstance(): Worker;
  }

  export abstract class LMLayerBase {
    /**
     * The underlying worker instance. By default, this is the LMLayerWorker. 
     */
    private _worker: Worker;
    /** Call this when the LMLayer has sent us the 'ready' message! */
    private _declareLMLayerReady: (conf: Configuration) => void;
    private _predictPromises: PromiseStore<Suggestion[]>;
    private _wordbreakPromises: PromiseStore<USVString>;
    private _nextToken: number;
    private capabilities: Capabilities;

    /**
     * Construct the top-level LMLayer interface. This also starts the underlying Worker.
     * 
     * @param uri URI of the underlying LMLayer worker code. This will usually be a blob:
     *            or file: URI. If uri is not provided, this will start the default Worker.
     */
    constructor(capabilities: Capabilities, workerFactory: WorkerFactory) {
      // Either use the given worker, or instantiate the default worker.
      this._worker = workerFactory.constructInstance();
      this._worker.onmessage = this.onMessage.bind(this)
      this._declareLMLayerReady = null;
      this._predictPromises = new PromiseStore;
      this._wordbreakPromises = new PromiseStore<USVString>();
      this._nextToken = Number.MIN_SAFE_INTEGER;

      this.sendConfig(capabilities);
    }

    /**
     * Initializes the LMLayer worker with the host platform's capability set.
     * 
     * @param capabilities The host platform's capability spec - a model cannot assume access to more context
     *                     than specified by this parameter.
     */
    private sendConfig(capabilities: Capabilities) {
      this._worker.postMessage({
        message: 'config',
        capabilities: capabilities
      });
    }

    /**
     * Initializes the LMLayer worker with a path to the desired model file.
     */
    loadModel(modelFilePath: string): Promise<Configuration> {
      return new Promise((resolve, _reject) => {
        // Sets up so the promise is resolved in the onMessage() callback, when it receives
        // the 'ready' message.
        this._declareLMLayerReady = resolve;

        this._worker.postMessage({
          message: 'load',
          model: modelFilePath
        });
      });
    }

    /**
     * Unloads the previously-active model from memory, resetting the LMLayer to prep
     * for transition to use of a new model.
     */
    public unloadModel() {
      this._worker.postMessage({
        message: 'unload'
      });
    }

    predict(transform: Transform | Distribution<Transform>, context: Context): Promise<Suggestion[]> {
      let token = this._nextToken++;
      return new Promise((resolve, reject) => {
        this._predictPromises.make(token, resolve, reject);
        this._worker.postMessage({
          message: 'predict',
          token: token,
          transform: transform,
          context: context,
        });
      });
    }

    wordbreak(context: Context): Promise<USVString> {
      let token = this._nextToken++;
      return new Promise((resolve, reject) => {
        this._wordbreakPromises.make(token, resolve, reject);
        this._worker.postMessage({
          message: 'wordbreak',
          token: token,
          context: context
        })
      });
    }

    // TODO: asynchronous close() method.
    //       Worker code must recognize message and call self.close().

    private onMessage(event: MessageEvent): void {
      let payload: OutgoingMessage = event.data;
      if (payload.message === 'error') {
        console.error(payload.log);
        if(payload.error) {
          console.error(payload.error);
        }
      }
      else if (payload.message === 'ready') {
        this._declareLMLayerReady(event.data.configuration);
      } else if (payload.message === 'suggestions') {
        this._predictPromises.keep(payload.token, payload.suggestions);
      } else if (payload.message === 'currentword') {
        this._wordbreakPromises.keep(payload.token, payload.word);
      } else {
        // This branch should never execute, but just in case...
        //@ts-ignore
        throw new Error(`Message not implemented: ${payload.message}`);
      }
    }

    /**
     * Clears out any computational resources in use by the LMLayer, including shutting
     * down any internal WebWorkers.
     */
    public shutdown() {
      this._worker.terminate();
    }

    /**
     * Given a function, this utility returns the source code within it, as a string.
     * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
     * build process.
     *
     * @param fn The function whose body will be returned.
     */
    static unwrap(fn: Function): string {
      let wrapper = fn.toString();
      let match = wrapper.match(/function[^{]+{((?:.|\r|\n)+)}[^}]*$/);
      return match[1];
    }
  }
}