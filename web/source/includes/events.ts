///<reference path="blank-module.ts" />
// Implements Node's EventEmitter class and related module components in a near
// browser-compatible way.  (Just requires a blank 'module' object on the window.)
///<reference path="../../node_modules/events/events.js" />

// Unfortunately, I can't get it to recognize type information properly
// because we can't use require statements.  So, a small-scale manual definition.
declare class EventEmitter {
  on(eventName: string, func: (...args: any[]) => boolean);

  emit(eventName: string, ...args: any[]);
}  