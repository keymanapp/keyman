/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { globalObject } from 'keyman/common/web-utils';
import { JSKeyboardInterface, ProcessorInitOptions } from 'keyman/engine/js-processor';
import { DefaultOutputRules, MinimalKeymanGlobal } from 'keyman/engine/keyboard';

export const DEFAULT_PROCESSOR_INIT_OPTIONS = {
  baseLayout: 'us',
  keyboardInterface:  new JSKeyboardInterface(globalObject(), MinimalKeymanGlobal),
  defaultOutputRules: new DefaultOutputRules()
} as ProcessorInitOptions;
