import * as baseTimeouts from './test-timeouts.mjs';

// General pattern, in case more are defined.
const scaledTimeouts = {
  ...baseTimeouts
};
Object.keys(baseTimeouts).forEach((key) => scaledTimeouts[key] *= 10);

export const DEFAULT_BROWSER_TIMEOUT = scaledTimeouts.DEFAULT_BROWSER_TIMEOUT; //ms