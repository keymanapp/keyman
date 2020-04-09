///<reference path="blank-module.ts" />
// Implements Node's EventEmitter class and related module components in a near
// browser-compatible way.  (Just requires a blank 'module' object on the window.)
///<reference path="../../node_modules/events/events.js" />

declare class EventEmitter {}  // Unfortunately, I can't get it to recognize type information properly
                               // because we can't use require statements.