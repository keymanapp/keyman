export { default as KeyboardProcessor } from "./keyboardProcessor.js";
export * from "./keyboardProcessor.js";
export { default as RuleBehavior } from "./ruleBehavior.js";
export * from './kbdInterface.js';
export { default as KeyboardInterface } from "./kbdInterface.js";
export * from "./systemStores.js";
export * from "./deadkeys.js";
export { default as OutputTarget } from "./outputTarget.js";
export * from "./outputTarget.js";
export { Mock } from "./mock.js";
export * from "./stringDivergence.js";

import { DeadkeyTracker } from './deadkeys.js';

/**
 * these are exported only for unit tests, do not use
 */
export const unitTestEndpoints = {
    DeadkeyTracker,
};
