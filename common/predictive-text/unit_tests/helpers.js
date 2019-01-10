/**
 * @file helpers
 * 
 * Globally-defined helper functions for use in in Mocha tests.
 */

// Choose the appropriate global object. Either `global` in
// Node, or `window` in browsers.
var _ = global || window;

/**
 * Creates a MessageEvent (for inter-worker communication), with the given data payload.
 *
 * @param {*} data 
 */
_.createMessageEventWithData = function createMessageEventWithData(data) {
  return { data };
}

/**
 * A valid model that suggests exactly what you want it to suggest.
 * 
 * @returns {ModelDescription}
 */
_.dummyModel = function dummyModel(futureSuggestions) {
  return {
    type: 'dummy',
    futureSuggestions: futureSuggestions || []
  };
}
/**
 * Capabilities of a keyboard that will ONLY send left-sided capabilities.
 * The keyboard does not support deleting to the right.
 *
 * @returns {Capabilities}
 */
_.defaultCapabilities = function defaultCapabilities() {
  return {
    maxLeftContextCodeUnits: 64
  };
}

/**
 * Returns the Context of an empty buffer; no text, at both the start and
 * end of the buffer.
 * 
 * @returns {Context}
 */
_.emptyContext = function emptyContext() {
  return {
    left: '',
    startOfBuffer: true,
    endOfBuffer: true
  };
}

/**
 * Returns a Transform that, when applied, makes no changes to the buffer.
 *
 * @returns {Transform}
 */
_.zeroTransform = function zeroTransform() {
  return {
    insert: '',
    deleteLeft: 0,
  };
}
