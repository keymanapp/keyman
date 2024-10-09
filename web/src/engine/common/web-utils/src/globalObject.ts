/**
 * Returns the base global object available to the current JS platform.
 * - In browsers, returns `window`.
 * - In WebWorkers, returns `self`.
 * - In Node, returns `global`.
 */
export default function getGlobalObject(): typeof globalThis {
  // Evergreen browsers have started defining 'globalThis'.
  // Refer to https://devblogs.microsoft.com/typescript/announcing-typescript-3-4/#type-checking-for-globalthis
  // and its referenced polyfill.  Said polyfill is very complex, so we opt for this far leaner variant.
  if(typeof globalThis != 'undefined') {
    return globalThis;  // Not available in IE or older Edge versions
    // @ts-ignore (TS will throw errors for whatever platform we're not compiling for.)
  } else if(typeof window != 'undefined') {
    // @ts-ignore
    return window; // The browser-based classic
    // @ts-ignore
  } else if(typeof self != 'undefined') {
    // @ts-ignore
    return self; // WebWorker global
  } else {
    // Assumption - if neither of the above exist, we're in Node, for unit-testing.
    // Node doesn't have as many methods and properties as the other two, but what
    // matters for us is that it's the base global.
    //
    // Some other headless JS solutions use 'this' instead, but Node's enough for our needs.
    // @ts-ignore
    return (global as any) as typeof globalThis;
  }
}