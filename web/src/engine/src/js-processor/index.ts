export { JSKeyboardProcessor, LogMessageHandler, ProcessorInitOptions } from "./jsKeyboardProcessor.js";
export { JSKeyboardInterface, KeyInformation, StoreNonCharEntry } from "./jsKeyboardInterface.js";
export { type ComplexKeyboardStore } from "./stores.js";


import { DefaultOutputRules } from 'keyman/engine/keyboard';
import { ProcessorInitOptions } from "./jsKeyboardProcessor.js";
export const unitTestEndpoints = {
  DEFAULT_OPTIONS: {
    baseLayout: 'us',
    defaultOutputRules: new DefaultOutputRules()
  } as ProcessorInitOptions
}
