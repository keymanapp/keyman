// Implements Node's EventEmitter class and related module components in a near
// browser-compatible way.  (Just requires a blank 'module' object on the window.)
///<reference path="../../../../../node_modules/eventemitter3/index.js" />

// Unfortunately, I can't get it to recognize type information properly
// because we can't use require statements.  So, a small-scale manual definition.
declare class EventEmitter {
  /** Add a listener for a given event */
  on(event: string, func: (...args: any[]) => void, context?: any);
  /** Add a one-time listener for a given event */
  once(event: string, func: (...args: any[]) => void, context?: any);
  removeListener(event: string, func: (...args: any[]) => void, context?: any, once?: boolean);
  removeAllListeners(event?: string);

  // Defines their alternately-themed aliases.
  addListener: typeof EventEmitter.prototype.on;
  off: typeof EventEmitter.prototype.removeListener;

  // Defines the actual event-raising function.
  emit(eventName: string, ...args: any[]);
}