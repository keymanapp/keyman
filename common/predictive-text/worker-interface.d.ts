// Necessary for headless compilation.  All contents were copied from the definitions in lib.dom.d.ts.

/** A message received by a target object. */
interface MessageEvent {
  /**
   * Returns the data of the message.
   */
  readonly data: any;
  /**
   * Returns the last event ID string, for server-sent events.
   */
  readonly lastEventId: string;
  /**
   * Returns the origin of the message, for server-sent events and cross-document messaging.
   */
  readonly origin: string;
  /**
   * Returns the MessagePort array sent with the message, for cross-document messaging and channel messaging.
   */
  readonly ports: ReadonlyArray<any>;
  /**
   * Returns the WindowProxy of the source window, for cross-document messaging, and the MessagePort being attached, in the connect event fired at SharedWorkerGlobalScope objects.
   */
  readonly source?: any;
}

interface PostMessageOptions {
  transfer?: any[];
}

/** This Web Workers API interface represents a background task that can be easily created and can send messages back to its creator. Creating a worker is as simple as calling the Worker() constructor and specifying a script to be run in the worker thread. */
interface Worker {
  onmessage: ((this: Worker, ev: MessageEvent) => any) | null;
  postMessage(message: any, transfer: any[]): void;
  postMessage(message: any, options?: PostMessageOptions): void;
  terminate(): void;
}