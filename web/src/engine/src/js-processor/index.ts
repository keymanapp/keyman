export { JSKeyboardProcessor, LogMessageHandler } from "./jsKeyboardProcessor.js";
export { JSKeyboardInterface, KeyInformation, StoreNonCharEntry } from "./jsKeyboardInterface.js";
export { ProcessorInitOptions } from './processorInitOptions.js';
export { type ComplexKeyboardStore } from "./stores.js";


import { DefaultOutputRules, globalObject, MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { ProcessorInitOptions } from './processorInitOptions.js';
import { JSKeyboardInterface } from './jsKeyboardInterface.js';
export const unitTestEndpoints = {
  DEFAULT_OPTIONS: {
    baseLayout: 'us',
    keyboardInterface:  new JSKeyboardInterface(globalObject(), MinimalKeymanGlobal),
    defaultOutputRules: new DefaultOutputRules()
  } as ProcessorInitOptions
}
