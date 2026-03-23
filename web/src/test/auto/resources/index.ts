export { assertingPromiseStatus } from './assertingPromiseStatus.js';
export { DEFAULT_PROCESSOR_INIT_OPTIONS } from './defaultProcessorInitOptions.js';
export { NodeKeyboardLoader } from './loader/nodeKeyboardLoader.js';
export { NodeCloudRequester } from './loader/nodeCloudRequester.js';
export { StageReportAssertion, SequenceAssertion, assertGestureSequence } from './sequenceAssertions.js';
export { simulateMultiSourceMatcherInput, simulateSelectorInput } from './simulateMultiSourceInput.js';

import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

export function getKeymanRoot(): string {
  const __dirname = dirname(fileURLToPath(import.meta.url));
  return __dirname + '/../../../../../../';
}

export function getWebTestResourcesPath(): string {
  return getKeymanRoot() + '/web/src/test/auto/resources';
}
