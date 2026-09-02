/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export { ContextManager, KeyboardCookie } from "./contextManager.js";
export { KeyboardDetails } from './keyboardDetails.js';
export { KeymanEngine } from './keymanEngine.js';
export { UIModule } from './uiModuleInterface.js';

//----------------------------------------------------------------------
// Exports for unit testing only

/** @internal */
export { type BrowserInitOptionSpec } from './configuration.js';
/** @internal */
export { type KeyboardInterface } from './keyboardInterface.js';

import { preprocessKeyboardEvent } from './hardwareEventKeyboard.js';
/** @internal */
export const unitTestEndPoints = {
  preprocessKeyboardEvent
}
