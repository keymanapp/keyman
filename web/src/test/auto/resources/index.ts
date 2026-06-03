/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
export { assertingPromiseStatus } from './assertingPromiseStatus.js';
export { DEFAULT_PROCESSOR_INIT_OPTIONS } from './defaultProcessorInitOptions.js';
export { NodeCloudRequester } from './loader/nodeCloudRequester.js';
export { NodeKeyboardLoader } from './loader/nodeKeyboardLoader.js';
export { assertGestureSequence, SequenceAssertion, StageReportAssertion } from './sequenceAssertions.js';
export { simulateMultiSourceMatcherInput, simulateSelectorInput } from './simulateMultiSourceInput.js';

import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

export function getKeymanRoot(): string {
  const __dirname = dirname(fileURLToPath(import.meta.url));
  return __dirname + '/../../../../../../';
}

export function getWebTestResourcesPath(): string {
  return getKeymanRoot() + '/web/src/test/auto/resources';
}
