// Necessary for headless compilation.  All contents were copied from the definitions in lib.dom.d.ts and trimmed for
// relevance.

/** A message received by a target object. */
interface MessageEvent<T = any> {
  /**
   * Returns the data of the message.
   */
  readonly data: T;

  // Other properties exist; we just don't use them yet.
}

interface PostMessageOptions {
  transfer?: any[];
}

/** This Web Workers API interface represents a background task that can be easily created and can send messages back to its creator. Creating a worker is as simple as calling the Worker() constructor and specifying a script to be run in the worker thread. */
interface Worker {
  onmessage: ((this: Worker, ev: MessageEvent) => any) | null;
  postMessage(message: any): void;
  terminate(): void;

  // addEventListener and removeEventListener also exist, but we don't utilize them.
}